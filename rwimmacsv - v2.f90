!=============================================================================!
! International Comprehensive Ocean-Atmosphere Data Set (ICOADS)  16 Nov 2015 !
! Filename:level: rwimma1:01D                     Fortran 90/77 program+shell !
! Purpose: Read and print/write IMMA                         Author: S.Lubker !
!=============================================================================!
! Software Revision Information (previous version: 20 Oct 2015, level 01C):
! CALL GETRPT modification removed from Performance comment.
!-----------------------------------------------------------------------3456789
! Filename:level: rwimmacvs:01A                   Fortran 90/77 program+shell !
! Purpose: Read and output IMMA data to a csv format        Author: R. Pushor !
!=============================================================================!
!
! Machine/language dependencies:  None known.  Non-ANSI features: common block
! containing both character and non-character variables, and specification
! statements after data statements.
! The program conforms to Fortran 90 source code conventions for comments and
! continuation lines, but still compiles without change in many Fortran 77
! environments.  Added/included source code can be either free form or fixed.
! For more information: See
! <http://icoads.noaa.gov/e-doc/imma/R2.5-imma_short.pdf> and
! <http://icoads.noaa.gov/ivad/IMMA-Rev.pdf> (electronic documents).
!-----------------------------------------------------------------------3456789
      PROGRAM RWIMMA
      IMPLICIT INTEGER(A-E,G-Z)
      INTEGER FNC,FM,FBSRC,FNR,FNI,FNE
!
      CHARACTER*14 PROGID
      DATA PROGID/'RWIMMACSV.01A '/
!
! CORE
      PARAMETER(YR=1,MO=2,DY=3,HR=4,LAT=5,LON=6,IM=7,ATTC=8,TI=9,LI=10        &
     &,DS=11,VS=12,NID=13,II=14,ID=15,C1=16,DI=17,D=18,WI=19,W=20,VI=21       &
     &,VV=22,WW=23,W1=24,SLP=25,A=26,PPP=27,IT=28,AT=29,WBTI=30,WBT=31        &
     &,DPTI=32,DPT=33,SI=34,SST=35,N=36,NH=37,CL=38,HI=39,H=40,CM=41          &
     &,CH=42,WD=43,WP=44,WH=45,SD=46,SP=47,SH=48)
!
!
      PARAMETER(NUM=60)
      CHARACTER*36 ANAM
	  CHARACTER*8 RPTID
      COMMON /IMMA1/IOFF(NUM),ILEN(NUM),ANAM(NUM),FUNITS(NUM),ITYPE(NUM),     &
	 &IOUT(NUM),ISEQ(NUM),RPTID
!
      CHARACTER*2048 RPT,CTRUE
      DIMENSION CTRUE(NUM),ITRUE(NUM),FTRUE(NUM)
      DATA CTRUE/NUM*' '/,ITRUE/NUM*-9999999/,FTRUE/NUM*-9999999./            &
     &,FMISS/-9999999./,FERR/-9999999./,UNIT/10/,STDOUT/6/
!
      CHARACTER*16 CREAN,CIVAD,CERROR
!
! PRINT .CAV FILE HEADER
!     WRITE(STDOUT,'(A)') '
     CALL PRNHDR(STDOUT,ILEN,ANAM,IOUT)
! INITIALIZE NUMBER OF REPORTS READ
      NREC=0
!
! READ REPORT
  100 CONTINUE
      READ(*,'(A)',END=900)RPT
! INCREMENT NUMBER OF REPORTS READ
      NREC=NREC+1
!
! CONVERT CHARACTERS TO FLOATING POINT VALUES
!      STOP 'Call GetRPT'
!      CALL GETRPT(RPT,CTRUE,ITRUE,FTRUE,FMISS,FERR                            &
!     &,ILEN,ANAM,FUNITS,ITYPE,IOUT,ISEQ,RPTID,NUM)
!
!      STOP 'Call Print Report'
! PRINT REPORT
!      WRITE(STDOUT,'(A)')TRIM(RPT(1:108))
      CALL PRNRPT(STDOUT,IOFF,ILEN,RPT,FUNITS,ITYPE,IOUT)
!
      GOTO 100
! END OF FILE
  900 CONTINUE
      END
!=============================================================================!
! WARNING:  Code beyond this point should not require any modification.       !
!=============================================================================!
!-----------------------------------------------------------------------3456789
      SUBROUTINE INIT(CTRUE,ITRUE,FTRUE,FMISS,ILEN,NUM)
! INITIALIZE CTRUE, ITRUE, FTRUE, NIVAD, AND NERROR
      IMPLICIT INTEGER(A-E,G-Z)
      CHARACTER*(*) CTRUE
      DIMENSION CTRUE(NUM),ITRUE(NUM),FTRUE(NUM),ILEN(NUM)
      CHARACTER*16 CREAN,CIVAD,CERROR
      COMMON /REAN/CREAN(15,100),IREAN(15,100),FREAN(15,100),NREAN
      COMMON /IVAD/CIVAD(19,100),IIVAD(19,100),FIVAD(19,100),NIVAD
      COMMON /ERROR/CERROR(9,100),IERROR(9,100),FERROR(9,100),NERROR
!
      DO 190 I=1,NUM
        IF (ILEN(I).EQ.1024) THEN
          CTRUE(I)(:)=' '
        ELSE
          IF (ILEN(I).GT.LEN(CTRUE(I))) STOP 'INIT LEN CTRUE'
          CTRUE(I)(:ILEN(I))=' '
        ENDIF
  190 CONTINUE
      ITRUE=NINT(FMISS)
      FTRUE=FMISS
!      CREAN=' '
!      IREAN=NINT(FMISS)
!      FREAN=FMISS
      NREAN=0
!      CIVAD=' '
!      IIVAD=NINT(FMISS)
!      FIVAD=FMISS
      NIVAD=0
!      CERROR=' '
!      IERROR=NINT(FMISS)
!      FERROR=FMISS
      NERROR=0
      END
!-----------------------------------------------------------------------3456789
      SUBROUTINE PRNSKP(BEG,END)
! DO NOT PRINT REPORT HEADER OR REPORT
      IMPLICIT INTEGER(A-E,G-Z)
      CHARACTER*(*) ANAM
      DIMENSION IOFF(*),ILEN(*),ANAM(*),FUNITS(*),ITYPE(*),IOUT(*)
      CHARACTER*2048 HDR,RPT,CSV
      EQUIVALENCE(HDR,CSV)
      DIMENSION SKP(1024)
      DATA SKP/1024*0/
      SAVE SKP
!
      DO 190 I=BEG,END
        SKP(I)=1
  190 CONTINUE
      RETURN
!-----------------------------------------------------------------------3456789
      ENTRY PRNHDR(STDOUT,ILEN,ANAM,IOUT)
! PRINT REPORT HEADER
!
    HDR=TRIM(ANAM(1))
    PTR=0
    DO 280 I=2,60
        IF (IOUT(I).EQ.1) THEN
!		    WRITE(STDOUT,'(A)')IOUT(I), ' ' , ILEN(i), ' ', TRIM(ANAM(I))
            HDR=TRIM(HDR)//', '//TRIM(ANAM(I))
        ENDIF
  280   CONTINUE
        WRITE(STDOUT,'(A)')TRIM(HDR)
      RETURN
!-----------------------------------------------------------------------3456789
      ENTRY PRNRPT(STDOUT,IOFF,ILEN,RPT,FUNITS,ITYPE,IOUT)
! PRINT REPORT
!	
    CSV=RPT(1:4)//'-'//RPT(5:6)//'-'//RPT(7:8)//','//RPT(9:10)//':'//RPT(11:12)//':00'
	IF (CSV(6:6).EQ.' ') THEN 
	    CSV(6:6)='0'
    ENDIF
	IF (CSV(9:9).EQ.' ') THEN 
	    CSV(9:9)='0'
    ENDIF
!	WRITE(STDOUT,'(A)')CSV
    DO 390 I=2,60
        IF (IOUT(I).EQ.1) THEN
			IF (RPT(IOFF(I):IOFF(I)+(ILEN(I)-1)).NE.' ') THEN
				SELECT CASE(ITYPE(I))
					CASE (1)
						IF (FUNITS(I).EQ.0.01) THEN
							CSV=TRIM(CSV)//','//RPT(IOFF(I):IOFF(I)+(ILEN(I)-3))//'.'//RPT(IOFF(I)+(ILEN(I)-2):IOFF(I)+(ILEN(I)-1))
						ELSE IF (FUNITS(I).EQ.0.1) THEN
							CSV=TRIM(CSV)//','//RPT(IOFF(I):IOFF(I)+(ILEN(I)-2))//'.'//RPT(IOFF(I)+(ILEN(I)-1):IOFF(I)+(ILEN(I)-1))
						ELSE
							CSV=TRIM(CSV)//','//RPT(IOFF(I):IOFF(I)+(ILEN(I)-1))
						END IF
					CASE (2)
						CSV=TRIM(CSV)//','//RPT(IOFF(I):IOFF(I)+(ILEN(I)-1))
					CASE (3)
						CSV=TRIM(CSV)//','//RPT(IOFF(I):IOFF(I)+(ILEN(I)-1))
					CASE DEFAULT
						STOP 'PRNRPT - Bad type'
			    END SELECT
            ELSE
			    CSV=TRIM(CSV)//','
		    ENDIF
        ENDIF
  390 CONTINUE
    WRITE(STDOUT,'(A)')CSV
    RETURN
    END
!-----------------------------------------------------------------------3456789
      SUBROUTINE GETRPT(RPT,CTRUE,ITRUE,FTRUE,FMISS,FERR                      &
     &,ILEN,ANAM,FUNITS,ITYPE,IOUT,ISEQ,RPTID,NUM)
! CONVERT CHARACTERS TO FLOATING POINT VALUES
      IMPLICIT INTEGER(A-E,G-Z)
      CHARACTER*(*) RPT,CTRUE,ANAM,RPTID
      DIMENSION CTRUE(NUM),ITRUE(NUM),FTRUE(NUM),ILEN(NUM),                   &
     &ANAM(NUM),FMIN1(NUM),FUNITS(NUM),ITYPE(NUM),IOUT(NUM),ISEQ(NUM)
      CHARACTER B36*10,IFMT(10)*8,STR*2048
      DATA IFMT/'(BZ,I1)','(BZ,I2)','(BZ,I3)','(BZ,I4)','(BZ,I5)'             &
     &,'(BZ,I6)','(BZ,I7)','(BZ,I8)','(BZ,I9)','(BZ,I10)'/
      SAVE IFMT
      CHARACTER UID*10
      DATA UID/'9815------'/
      SAVE UID
      CHARACTER*16 CREAN,CIVAD,CERROR
      COMMON /REAN/CREAN(15,100),IREAN(15,100),FREAN(15,100),NREAN
      COMMON /IVAD/CIVAD(19,100),IIVAD(19,100),FIVAD(19,100),NIVAD
      COMMON /ERROR/CERROR(9,100),IERROR(9,100),FERROR(9,100),NERROR
      PARAMETER(ATTI1=49)
      PARAMETER(ATTI5=100)
      PARAMETER(ATTI6=165)
      PARAMETER(ATTI7=186)
      PARAMETER(ATTI8=209)
      PARAMETER(ATTI9=234)
      PARAMETER(ATTI95=251)
      PARAMETER(ATTI96=266)
      PARAMETER(ATTI97=285)
      PARAMETER(ATTI98=294)
      PARAMETER(ATTI99=302)
!
!      IF (INITICN(ANAM,FMIN1,NUM).NE.ATTI99) STOP 'GETRPT INITICN'
      IF (RPT(:10).NE.UID) CALL INIT(CTRUE,ITRUE,FTRUE,FMISS,ILEN,NUM)
      PTR=0
      IF (RPT(:4).NE.'9815') CALL GETATT(RPT,PTR,1,48)
      ATTC=0
      DO WHILE (PTR.LT.LEN(RPT) .AND. RPT(PTR+1:).NE.' ')
        SELECT CASE(RPT(PTR+1:PTR+4))
          CASE(' 276')
            TRS=0
            IF (PTR+76.GT.LEN(RPT)) STOP 'GETRPT LEN RPT'
            STR=' 594'//RPT(PTR+5:PTR+6)//RPT(PTR+8:PTR+8)//' '               &
     &      //RPT(PTR+9:PTR+10)//RPT(PTR+15:PTR+35)//' '                      &
     &      //RPT(PTR+36:PTR+76)//'                     '
            CALL GETATT(STR,TRS,ATTI5,ATTI5-1+65)
            PTR=PTR+76
          CASE(' 366')
            TRS=0
            IF (PTR+66.GT.LEN(RPT)) STOP 'GETRPT LEN RPT'
            STR=' 668'//RPT(PTR+5:PTR+14)//' '//RPT(PTR+15:PTR+45)            &
     &      //RPT(PTR+47:PTR+50)//' '//RPT(PTR+51:PTR+51)//' '                &
     &      //RPT(PTR+52:PTR+66)
            CALL GETATT(STR,TRS,ATTI6,ATTI6-1+21)
            PTR=PTR+66
          CASE(' 457')
            TRS=0
            IF (PTR+57.GT.LEN(RPT)) STOP 'GETRPT LEN RPT'
            STR=' 758'//' '//RPT(PTR+5:PTR+57)
            CALL GETATT(STR,TRS,ATTI7,ATTI7-1+23)
            PTR=PTR+57
          CASE DEFAULT
            STOP 'GETRPT ATTI ATTL'
        END SELECT
      ENDDO
      RETURN
!-----------------------------------------------------------------------3456789
      ENTRY PUTRPT(RPT,CTRUE,ITRUE,FTRUE,FMISS,FERR                           &
     &,ILEN,ANAM,FMIN1,FUNITS,ITYPE,RPTID,NUM)
! CONVERT FLOATING POINT VALUES TO CHARACTERS
!
      IF (INITICN(ANAM,FMIN1,NUM).NE.ATTI99) STOP 'PUTRPT INITICN'
      RPT=' '
      PTR=0
      CALL PUTATT(RPT,PTR,1,48)
      ATTC=0
!      CALL PUTATT(RPT,PTR,ATTI1,ATTI1-1+51)
!      CALL PUTATT(RPT,PTR,ATTI5,ATTI5-1+65)
!      CALL PUTATT(RPT,PTR,ATTI6,ATTI6-1+21)
!      CALL PUTATT(RPT,PTR,ATTI7,ATTI7-1+23)
!      CALL PUTATT(RPT,PTR,ATTI8,ATTI8-1+25)
!      CALL PUTATT(RPT,PTR,ATTI9,ATTI9-1+17)
!      CALL PUTATT(RPT,PTR,ATTI95,ATTI95-1+15)
!      CALL PUTATT(RPT,PTR,ATTI96,ATTI96-1+19)
!      CALL PUTATT(RPT,PTR,ATTI97,ATTI97-1+9)
!      CALL PUTATT(RPT,PTR,ATTI98,ATTI98-1+8)
!      CALL PUTATT(RPT,PTR,ATTI99,ATTI99-1+4)
      RPT(24:25)=' 1'
      RPT(26:26)=B36(ATTC,'*',1)
      RETURN
!-----------------------------------------------------------------------3456789
      ENTRY PUTSUB(RPT,CTRUE,ITRUE,FTRUE,FMISS,FERR                           &
     &,ILEN,ANAM,FMIN1,FUNITS,ITYPE,RPTID,NUM,BEG,END)
! CONVERT SUBSIDIARY FLOATING POINT VALUES TO CHARACTERS
!
      IF (INITICN(ANAM,FMIN1,NUM).NE.ATTI99) STOP 'PUTSUB INITICN'
      RPT=' '
      PTR=15
      ATTC=0
      IF (BEG.LT.ATTI95) THEN
        CALL PUTATT(RPT,PTR,BEG,END)
      ELSE IF (BEG.EQ.ATTI95) THEN
        DO J=1,NREAN
          CTRUE(ATTI95:ATTI95-1+15)(:16)=CREAN(:,J)
          FTRUE(ATTI95:ATTI95-1+15)=FREAN(:,J)
          PN=GETPN(NINT(FTRUE(ATTI95-1+3)),NINT(FTRUE(ATTI95-1+4)))
          CALL SETINHR(PN)
          CALL PUTATT(RPT,PTR,ATTI95,ATTI95-1+15)
          IREAN(:,J)=ITRUE(ATTI95:ATTI95-1+15)
          CREAN(:,J)=CTRUE(ATTI95:ATTI95-1+15)
        ENDDO
      ELSE IF (BEG.EQ.ATTI96) THEN
        DO J=1,NIVAD
          CTRUE(ATTI96:ATTI96-1+19)(:16)=CIVAD(:,J)
          FTRUE(ATTI96:ATTI96-1+19)=FIVAD(:,J)
          PN=GETPN(NINT(FTRUE(ATTI96-1+3)),NINT(FTRUE(ATTI96-1+4)))
          CALL SETINHI(PN)
          CALL PUTATT(RPT,PTR,ATTI96,ATTI96-1+19)
          IIVAD(:,J)=ITRUE(ATTI96:ATTI96-1+19)
          CIVAD(:,J)=CTRUE(ATTI96:ATTI96-1+19)
        ENDDO
      ELSE IF (BEG.EQ.ATTI97) THEN
        DO J=1,NERROR
          CTRUE(ATTI97:ATTI97-1+9)(:16)=CERROR(:,J)
          FTRUE(ATTI97:ATTI97-1+9)=FERROR(:,J)
          PN=GETPN(NINT(FTRUE(ATTI97-1+3)),NINT(FTRUE(ATTI97-1+4)))
          CALL SETINHE(PN)
          CALL PUTATT(RPT,PTR,ATTI97,ATTI97-1+9)
          IERROR(:,J)=ITRUE(ATTI97:ATTI97-1+9)
          CERROR(:,J)=CTRUE(ATTI97:ATTI97-1+9)
        ENDDO
      ENDIF
      IF (PTR.GT.15) THEN
        PTR=0
        CALL PUTATT(RPT,PTR,ATTI98,ATTI98-1+8)
      ENDIF
      RETURN
!-----------------------------------------------------------------------
      CONTAINS
!-----------------------------------------------------------------------3456789
      SUBROUTINE GETATT(STR,TRS,BEG,END)
! GET IN STR AT TRS THE ATTACHMENT BEG THRU END
      IMPLICIT INTEGER(A-E,G-Z)
      CHARACTER*(*)STR
      INTEGER I
!
      DO I=BEG,END
        IF (ILEN(I).NE.1024) THEN
          IF (ILEN(I).GT.LEN(CTRUE(I))) STOP 'GETATT LEN CTRUE'
          IF (TRS+ILEN(I).GT.LEN(STR)) STOP 'GETATT LEN RPT'
          CTRUE(I)(:ILEN(I))=STR(TRS+1:TRS+ILEN(I))
          TRS=TRS+ILEN(I)
        ELSE
          CTRUE(I)(:)=STR(TRS+1:)
          IF (CTRUE(I)(:).NE.STR(TRS+1:)) STOP 'GETATT LEN SUPD'
          TRS=TRS+LEN_TRIM(STR(TRS+1:))
        ENDIF
!
        IF (CTRUE(I)(:MIN(ILEN(I),LEN(CTRUE(I)))).EQ.' ') THEN
          ITRUE(I)=NINT(FMISS)
        ELSE IF (ITYPE(I).EQ.1) THEN
          READ(CTRUE(I)(:ILEN(I)),IFMT(ILEN(I)),IOSTAT=IOS)ITRUE(I)
          IF (IOS.NE.0) ITRUE(I)=NINT(FERR)
        ELSE IF (ITYPE(I).EQ.2) THEN
          ITRUE(I)=IB36(CTRUE(I)(:ILEN(I)),NINT(FERR),ILEN(I))
        ELSE
          ITRUE(I)=ICHAR(CTRUE(I)(:1))
        ENDIF
!
        IF (ITRUE(I).EQ.NINT(FMISS)) THEN
          FTRUE(I)=FMISS
        ELSE IF (ITRUE(I).EQ.NINT(FERR)) THEN
          FTRUE(I)=FERR
        ELSE IF (ITYPE(I).LE.2) THEN
          FTRUE(I)=ITRUE(I)*FUNITS(I)
        ELSE
          FTRUE(I)=ITRUE(I)
        ENDIF
      ENDDO
      ATTC=ATTC+1
      END SUBROUTINE GETATT
!-----------------------------------------------------------------------3456789
      SUBROUTINE PUTATT(STR,TRS,BEG,END)
! PUT IN STR AT TRS THE ATTACHMENT BEG THRU END
      IMPLICIT INTEGER(A-E,G-Z)
      CHARACTER*(*)STR
      INTEGER I
!
      IF (ADJUSTL(ANAM(BEG)).EQ.'ATTI') THEN
        IF (ALL(FTRUE(BEG+2:END).EQ.FMISS)) RETURN
        FTRUE(BEG)=FMIN1(BEG)
        FTRUE(BEG+1)=FMIN1(BEG+1)
      ENDIF
      DO I=BEG,END
        IF (FTRUE(I).EQ.FMISS) THEN
          ITRUE(I)=NINT(FMISS)
        ELSE IF (FTRUE(I).EQ.FERR) THEN
          ITRUE(I)=NINT(FERR)
        ELSE IF (ITYPE(I).LE.2) THEN
          ITRUE(I)=NINT(FTRUE(I)/FUNITS(I))
        ELSE
          ITRUE(I)=NINT(FTRUE(I))
        ENDIF
!
        IF (ITRUE(I).EQ.NINT(FMISS)) THEN
          CTRUE(I)(:MIN(ILEN(I),LEN(CTRUE(I))))=' '
        ELSE IF (ITYPE(I).EQ.1) THEN
          WRITE(CTRUE(I)(:ILEN(I)),IFMT(ILEN(I)))ITRUE(I)
        ELSE IF (ITYPE(I).EQ.2) THEN
          CTRUE(I)(:ILEN(I))=B36(ITRUE(I),'*',ILEN(I))
        ELSE
!          CTRUE(I)(:ILEN(I))=REPEAT(CHAR(ITRUE(I)),16)
        ENDIF
!
        IF (ILEN(I).NE.1024) THEN
          IF (ILEN(I).GT.LEN(CTRUE(I))) STOP 'PUTATT LEN CTRUE'
          IF (TRS+ILEN(I).GT.LEN(STR)) STOP 'PUTATT LEN RPT'
          STR(TRS+1:TRS+ILEN(I))=CTRUE(I)(:ILEN(I))
          TRS=TRS+ILEN(I)
        ELSE
          STR(TRS+1:)=CTRUE(I)(:)
          IF (CTRUE(I)(:).NE.STR(TRS+1:)) STOP 'PUTATT LEN SUPD'
          TRS=TRS+LEN_TRIM(STR(TRS+1:))
        ENDIF
      ENDDO
      ATTC=ATTC+1
      END SUBROUTINE PUTATT
!-----------------------------------------------------------------------3456789
      SUBROUTINE SETINHR(PN)
! SET REANALYSES ATTACHMENT INHERITED ATTRIBUTES
      INTEGER I
      DO I=0,3
        FMIN1(ATTI95-1+8+I*NUM)=FMIN1(PN+I*NUM)
        FMIN1(ATTI95-1+10+I*NUM)=FMIN1(PN+I*NUM)
        FMIN1(ATTI95-1+12+I*NUM)=FMIN1(PN+I*NUM)
      ENDDO
      FUNITS(ATTI95-1+8:ATTI95-1+12)=FUNITS(PN)/10.
      END SUBROUTINE SETINHR
!-----------------------------------------------------------------------3456789
      SUBROUTINE SETINHI(PN)
! SET IVAD ATTACHMENT INHERITED ATTRIBUTES
      INTEGER I
      DO I=0,3
        FMIN1(ATTI96-1+6+I*NUM)=FMIN1(PN+I*NUM)
      ENDDO
      WHERE (FTRUE(ATTI96-1+5:ATTI96-1+14:3).NE.FMISS)
        FUNITS(ATTI96-1+6:ATTI96-1+15:3)=1./(10.**                            &
     &   FTRUE(ATTI96-1+5:ATTI96-1+14:3))
      ELSEWHERE
        FUNITS(ATTI96-1+6:ATTI96-1+15:3)=1.
      ENDWHERE
      END SUBROUTINE SETINHI
!-----------------------------------------------------------------------3456789
      SUBROUTINE SETINHE(PN)
! SET ERROR ATTACHMENT INHERITED ATTRIBUTES
      INTEGER I
      DO I=0,3
        FMIN1(ATTI97-1+6+I*NUM)=FMIN1(PN+I*NUM)
      ENDDO
      FUNITS(ATTI97-1+6)=FUNITS(PN)
      ITYPE(ATTI97-1+6)=ITYPE(PN)
      END SUBROUTINE SETINHE
      END
!-----------------------------------------------------------------------3456789
      INTEGER FUNCTION INITICN(ANAM,FMIN1,NUM)
! INITIALIZE PARAMETER TABLE AND COMPONENT NUMBER AND FIELD NUMBER LISTS
      IMPLICIT INTEGER(A-E,G-Z)
      INTEGER FN
      CHARACTER*(*) ANAM
      DIMENSION ANAM(NUM),FMIN1(NUM)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: LIST
      INTEGER, DIMENSION(0:99,99) :: TABLE=0
      SAVE LIST,TABLE
!
      IF (ADJUSTL(ANAM(1)).NE.'YR') STOP 'INITICN 1ST'
      IF (ADJUSTL(ANAM(NUM)).NE.'SUPD') STOP 'INITICN NUM'
      IF (TABLE(99,1).EQ.0) CALL INIT()
      INITICN=TABLE(99,1)
      RETURN
!-----------------------------------------------------------------------3456789
      ENTRY GETPN(ICN,FN)
! PARAMETER NUMBER OF A COMPONENT NUMBER AND A FIELD NUMBER
      IF (TABLE(99,1).EQ.0) STOP 'GETPN TABLE'
      IF (ICN.LT.0 .OR. ICN.GT.99) STOP 'GETPN ICN'
      IF (FN.LT.1 .OR. FN.GT.99) STOP 'GETPN FN'
      IF (TABLE(ICN,FN).EQ.0) STOP 'GETPN ICN FN'
      GETPN=TABLE(ICN,FN)
      RETURN
!-----------------------------------------------------------------------3456789
      ENTRY GETICN(PN)
! COMPONENT NUMBER OF A PARAMETER NUMBER
      IF (TABLE(99,1).EQ.0) STOP 'GETICN TABLE'
      IF (PN.LT.1 .OR. PN.GT.UBOUND(LIST,2)) STOP 'GETICN PN'
      GETICN=LIST(1,PN)
      RETURN
!-----------------------------------------------------------------------3456789
      ENTRY GETFN(PN)
! FIELD NUMBER OF A PARAMETER NUMBER
      IF (TABLE(99,1).EQ.0) STOP 'GETFN TABLE'
      IF (PN.LT.1 .OR. PN.GT.UBOUND(LIST,2)) STOP 'GETFN PN'
      GETFN=LIST(2,PN)
      RETURN
!-----------------------------------------------------------------------3456789
      CONTAINS
!-----------------------------------------------------------------------3456789
        SUBROUTINE INIT()
! SEE INITICN
        INTEGER ICN,FN,PN
!
        ALLOCATE(LIST(2,NUM))
        ICN=0
        FN=0
        DO I=1,NUM
          IF (ADJUSTL(ANAM(I)).EQ.'ATTI') THEN
            ICN=NINT(FMIN1(I))
            FN=0
          ENDIF
          FN=FN+1
          LIST(1,I)=ICN
          LIST(2,I)=FN
          TABLE(ICN,FN)=I
        ENDDO
        END SUBROUTINE INIT
      END
!-----------------------------------------------------------------------3456789
      SUBROUTINE MINMAX(CTRUE,ITRUE,FTRUE,FMISS,FERR                          &
     &,ILEN,FMIN1,FMAX1,FMIN2,FMAX2,FUNITS,ITYPE,NUM)
! SET EXTREME FLOATING POINT VALUES TO ERROR VALUE
      IMPLICIT INTEGER(A-E,G-Z)
      CHARACTER*(*) CTRUE
      DIMENSION CTRUE(NUM),ITRUE(NUM),FTRUE(NUM)                              &
     &,ILEN(NUM),FMIN1(NUM),FMAX1(NUM),FMIN2(NUM),FMAX2(NUM)                  &
     &,FUNITS(NUM),ITYPE(NUM)
!
      DO 190 I=1,NUM
        IF (ITYPE(I).LE.2) THEN
          IF (FTRUE(I).EQ.FMISS .OR. FTRUE(I).EQ.FERR) THEN
            ITRUE(I)=NINT(FTRUE(I))
            GOTO 190
          ENDIF
          ITRUE(I)=NINT(FTRUE(I)/FUNITS(I))
          IF (ITRUE(I).GE.NINT(FMIN1(I)/FUNITS(I))                            &
     &   .AND.ITRUE(I).LE.NINT(FMAX1(I)/FUNITS(I))                            &
     &   .OR. FMIN2(I).NE.FMISS                                               &
     &   .AND.ITRUE(I).GE.NINT(FMIN2(I)/FUNITS(I))                            &
     &   .AND.ITRUE(I).LE.NINT(FMAX2(I)/FUNITS(I))) THEN
          ELSE
            FTRUE(I)=FERR
            ITRUE(I)=NINT(FERR)
          ENDIF
        ELSE
          IF (ILEN(I).EQ.1024) THEN
            LENGTH=LEN(CTRUE(I))
          ELSE
            IF (ILEN(I).GT.LEN(CTRUE(I))) STOP 'MINMAX LEN CTRUE'
            LENGTH=ILEN(I)
          ENDIF
          IF (CTRUE(I)(:LENGTH).EQ.' ') THEN
            FTRUE(I)=FMISS
            ITRUE(I)=NINT(FMISS)
            GOTO 190
          ENDIF
          FTRUE(I)=ICHAR(CTRUE(I)(:1))
          ITRUE(I)=NINT(FTRUE(I))
          DO 180 J=1,LENGTH
            IF (LGE(CTRUE(I)(J:J),'A')                                        &
     &     .AND.LLE(CTRUE(I)(J:J),'Z')                                        &
     &     .OR. NINT(FMIN1(I)).LE.48                                          &
     &     .AND.LGE(CTRUE(I)(J:J),'0')                                        &
     &     .AND.LLE(CTRUE(I)(J:J),'9')                                        &
     &     .OR. NINT(FMIN1(I)).EQ.32                                          &
     &     .AND.LGE(CTRUE(I)(J:J),' ')                                        &
     &     .AND.LLE(CTRUE(I)(J:J),'~')) THEN
            ELSE
              FTRUE(I)=FERR
              ITRUE(I)=NINT(FERR)
              GOTO 190
            ENDIF
  180     CONTINUE
        ENDIF
  190 CONTINUE
      END
!-----------------------------------------------------------------------3456789
      SUBROUTINE SAVSUM(CTRUE,FTRUE,FMISS,FERR                                &
     &,ILEN,ANAM,NUM)
! SAVE SUMMARY INFORMATION
      IMPLICIT INTEGER(A-E,G-Z)
      CHARACTER*(*) PROGID,ANAM,CTRUE
      DIMENSION CTRUE(NUM),FTRUE(NUM),ILEN(NUM),ANAM(NUM)
      CHARACTER*64 STR
      DIMENSION SUM1(1024),SUM2(1024),SUM3(1024)
      DATA SUM1,SUM2,SUM3/1024*0,1024*0,1024*0/
      SAVE SUM1,SUM2,SUM3
      PC(A1,A2)=NINT(A1*100./MAX(A2,1))
!
      DO 190 I=1,NUM
        IF (FTRUE(I).NE.FMISS .AND. FTRUE(I).NE.FERR) THEN
          SUM1(I)=SUM1(I)+1
        ELSE
          SUM2(I)=SUM2(I)+1
          IF (FTRUE(I).EQ.FMISS) THEN
            IF (ILEN(I).EQ.1024) THEN
              IF (CTRUE(I)(:).EQ.' ') GOTO 190
            ELSE
              IF (ILEN(I).GT.LEN(CTRUE(I))) STOP 'SAVSUM LEN CTRUE'
              IF (CTRUE(I)(:ILEN(I)).EQ.' ') GOTO 190
            ENDIF
          ENDIF
          SUM3(I)=SUM3(I)+1
          WRITE(STR,'(I3,1X,A5,2X,A53)')I,ANAM(I),CTRUE(I)
          CALL SAVSTR(STR)
        ENDIF
  190 CONTINUE
      RETURN
!-----------------------------------------------------------------------3456789
      ENTRY PRNSUM(UNIT,PROGID,ANAM,NUM)
! PRINT SUMMARY INFORMATION TO UNIT
!
      WRITE(UNIT,'(A)')PROGID
      WRITE(UNIT,'(/1X,A)')'SUMMARY OF FIELDS'
      WRITE(UNIT,'(A6,2(2X,A10,A11,A13))')'FIELD'                             &
     &,'# EXTANT','# MISSING','# ERRONEOUS'                                   &
     &,'% EXTANT','% MISSING','% ERRONEOUS'
      DO 290 I=1,NUM
        IF (SUM1(I).GT.0 .OR. SUM3(I).GT.0) THEN
          TOTAL=SUM1(I)+SUM2(I)
          WRITE(UNIT,'(1X,A5,2(2X,I10,I11,I13))')ANAM(I)                      &
     &    ,   SUM1(I)       ,   SUM2(I)       ,   SUM3(I)                     &
     &    ,PC(SUM1(I),TOTAL),PC(SUM2(I),TOTAL),PC(SUM3(I),TOTAL)
        ENDIF
        SUM1(I)=0
        SUM2(I)=0
        SUM3(I)=0
  290 CONTINUE
      WRITE(UNIT,'(/1X,A)')'SUMMARY OF ERRORS'
      WRITE(UNIT,'(A6,2X,A,T70,A)')'FIELD','ERROR','FREQUENCY'
      CALL PRNSTR(UNIT)
      END
!-----------------------------------------------------------------------3456789
      SUBROUTINE SAVSTR(STR)
! SAVE FREQUENCY OF STRING
      IMPLICIT INTEGER(A-E,G-Z)
      PARAMETER(MMAX=10000)
      CHARACTER*64 STR,ARR1
      DIMENSION ARR1(MMAX),ARR2(MMAX)
      DATA M/0/
      SAVE
!
      DO 190 I=1,M
        IF (STR.NE.ARR1(I)) GOTO 190
        ARR2(I)=ARR2(I)+1
        RETURN
  190 CONTINUE
      IF (M+1.GE.MMAX) STOP 'SAVSTR INCREASE MMAX'
      M=M+1
      ARR1(M)=STR
      ARR2(M)=1
      RETURN
!-----------------------------------------------------------------------3456789
      ENTRY PRNSTR(UNIT)
! PRINT FREQUENCY OF STRING
!
      DO 290 K=1,M-1
        J=K
        DO 280 L=K+1,M
          IF (LLT(ARR1(L),ARR1(J))) J=L
  280   CONTINUE
        IF (J.NE.K) THEN
          ARR1(MMAX)=ARR1(K)
          ARR2(MMAX)=ARR2(K)
          ARR1(K)=ARR1(J)
          ARR2(K)=ARR2(J)
          ARR1(J)=ARR1(MMAX)
          ARR2(J)=ARR2(MMAX)
        ENDIF
  290 CONTINUE
      WRITE(UNIT,'(A,T70,I9)')(ARR1(I)(4:),ARR2(I),I=1,M)
      M=0
      END
!-----------------------------------------------------------------------3456789
      FUNCTION IB36(B36,IERR,ILEN)
! CONVERT FROM BASE 36 CHARACTER STRING TO INTEGER
      IMPLICIT INTEGER(A-Z)
      CHARACTER (LEN=*) B36,STR
      PARAMETER(STR='0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ')
!
      IB36=IERR
      J=VERIFY(B36(:ILEN),' ')
      IF (J.EQ.0 .OR. ILEN+1-J.GT.5) RETURN
      IF (VERIFY(B36(J:ILEN),STR).NE.0) RETURN
!
      IB36=0
      INT=1
      DO J=ILEN,1,-1
        IF (B36(:J).EQ.' ') EXIT
        IB36=(INDEX(STR,B36(J:J))-1)*INT+IB36
        INT=INT*36
      ENDDO
      END
!-----------------------------------------------------------------------3456789
      FUNCTION B36(IB36,ERR,ILEN)
! CONVERT FROM INTEGER TO BASE 36 CHARACTER STRING
      IMPLICIT INTEGER(A-Z)
      CHARACTER (LEN=*) B36,STR
      PARAMETER(STR='0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ')
      CHARACTER ERR
!
      B36=REPEAT(ERR,ILEN)
      J=MIN(ILEN,5)
      IF (IB36.LT.0 .OR. IB36.GT.36**J-1) RETURN
!
      B36=REPEAT(' ',ILEN-1)//'0'
      INT=1
      DO J=ILEN,1,-1
        IF (IB36/INT.EQ.0) EXIT
        B36(J:J)=STR(MOD(IB36/INT,36)+1:)
        INT=INT*36
      ENDDO
      END
!-----------------------------------------------------------------------3456789
      SUBROUTINE GETUID(CTRUE)
! GET UNIQUE REPORT ID
      CHARACTER*6 CTRUE
!
      OPEN(98,FILE='LAST_UID')
      READ(98,'(A)')CTRUE
      CALL B36INC(CTRUE)
      REWIND(98)
      WRITE(98,'(A)')CTRUE
      CLOSE(98)
      END
!-----------------------------------------------------------------------3456789
      SUBROUTINE B36INC(B36)
! INCREMENT BASE 36 CHARACTER STRING
      IMPLICIT INTEGER(A-Z)
      CHARACTER (LEN=*) B36,STR
      PARAMETER(STR='0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ')
!
      IF (VERIFY(B36,STR).EQ.0) THEN
        DO J=LEN(B36),1,-1
          NDX=INDEX(STR,B36(J:J))
          IF (NDX.LT.36) THEN
            B36(J:J)=STR(NDX+1:NDX+1)
            RETURN
          ELSE
            B36(J:J)=STR(1:1)
          ENDIF
        ENDDO
      ENDIF
      STOP 'B36INC'
      END
!-----------------------------------------------------------------------3456789
      SUBROUTINE EAST(ITRUE,FTRUE,FERR)
! CONVERT LONGITUDE TO DEGREES EAST
      IMPLICIT INTEGER(A-E,G-Z)
!
      ITRUE=NINT(FTRUE/0.01)
      IF(ITRUE.GE.-36000 .AND. ITRUE.LE.36000)THEN
        ITRUE=MOD(ITRUE+36000,36000)
        FTRUE=ITRUE*0.01
      ELSE
        ITRUE=NINT(FERR)
        FTRUE=FERR
      ENDIF
      END
!-----------------------------------------------------------------------3456789
      BLOCK DATA BDIMMA
! COMMON BLOCK DATA STATEMENTS
      IMPLICIT INTEGER(A-E,G-Z)
!
!     missing = -9999999
!     ILEN    = field length
!     ANAM    = field ANAMeviationg)
!     FUNITS  = field units (missing, if itype=3)
!     ITYPE   = 1: numeric:   decimal range(s)
!               2: numeric:   decimal range(s), transformed to base36
!               3: character: decimal range(s), transformed to ascii
!     RPTID   = 1 (IMMA version that the program can write)
!
      PARAMETER(NUM=60)
      CHARACTER*36 ANAM
	  CHARACTER*8 RPTID
      COMMON /IMMA1/IOFF(NUM),ILEN(NUM),ANAM(NUM),FUNITS(NUM),ITYPE(NUM),     &
	 &IOUT(NUM),ISEQ(NUM),RPTID
      DATA RPTID/' 1'/
      CHARACTER*16 CREAN,CIVAD,CERROR
      COMMON /REAN/CREAN(15,100),IREAN(15,100),FREAN(15,100),NREAN
      COMMON /IVAD/CIVAD(19,100),IIVAD(19,100),FIVAD(19,100),NIVAD
      COMMON /ERROR/CERROR(9,100),IERROR(9,100),FERROR(9,100),NERROR
      DATA NREAN,NIVAD,NERROR/3*0/
!
      DATA (IOFF(I),ILEN(I),ANAM(I),FUNITS(I),ITYPE(I),IOUT(I),ISEQ(I),       &
	 &I=1,60)/                                                                &
     &   001, 12,'Date of Observation, Time of Obsser ', 1.      , 1, 1,  4,  &
     &   013, 05,'Latitude                            ', 0.01    , 1, 1,  2,  &
     &   018, 06,'Longitude                           ', 0.01    , 1, 1,  3,  &
     &   024, 02,'IMMA version                        ', 1.      , 1, 0,  0,  &
     &   026, 01,'attm count                          ', 1.      , 2, 0,  0,  &
     &   027, 01,'time indicator                      ', 1.      , 1, 0,  0,  &
     &   028, 01,'latitude/longitude indicator        ', 1.      , 1, 0,  0,  &
     &   029, 01,'ship course                         ', 1.      , 1, 0,  0,  &
     &   030, 01,'ship speed                          ', 1.      , 1, 0,  0,  &
     &   031, 02,'national source indicator           ', 1.      , 1, 0,  0,  &
     &   033, 02,'ID indicator                        ', 1.      , 1, 0,  0,  &
     &   035, 09,'Identification                      ',-9999999., 3, 1, 13,  &
     &   044, 02,'country code                        ',-9999999., 3, 1, 14,  &
     &   046, 01,'wind direction indicator            ', 1.      , 1, 0,  0,  &
     &   047, 03,'Wind Direction                      ', 1.      , 1, 1, 32,  &
     &   050, 01,'wind speed indicator                ', 1.      , 1, 0,  0,  &
     &   051, 03,'Wind Speed                          ', 0.1     , 1, 1, 33,  &
     &   054, 01,'Visibility Indicator                ', 1.      , 1, 1, 29,  &
     &   055, 02,'Visibility                          ', 1.      , 1, 1, 28,  &
     &   057, 02,'Present Weather                     ', 1.      , 1, 1, 30,  &
     &   059, 01,'Past Weather                        ', 1.      , 1, 1, 31,  &
     &   060, 05,'Sea Level Pressure                  ', 0.1     , 1, 1, 08,  &
     &   065, 01,'Characteristics of Pressure Tendency', 1.      , 1, 1, 09,  &
     &   066, 03,'Pressure Tendency                   ', 0.1     , 1, 1, 10,  &
     &   069, 01,'indicator for temperatures          ', 1.      , 1, 0,  0,  &
     &   070, 04,'Air Temperature                     ', 0.1     , 1, 1, 11,  &
     &   074, 01,'WBT indicator                       ', 1.      , 1, 0,  0,  &
     &   075, 04,'Wet Bulb Temperature                ', 0.1     , 1, 1, 12,  &
     &   079, 01,'DPT indic                           ', 1.      , 1, 0,  0,  &
     &   080, 04,'Dew Point Temperature               ', 0.1     , 1, 1, 13,  &
     &   084, 02,'SST measurement method              ', 1.      , 1, 0,  0,  &
     &   086, 04,'Sea Surface Temperature             ', 0.1     , 1, 1, 14,  &
     &   090, 01,'Total Cloud Amount                  ', 1.      , 1, 1, 21,  &
     &   091, 01,'Low Cloud Amount                    ', 1.      , 1, 1, 22,  &
     &   092, 01,'Low Cloud Type                      ', 1.      , 2, 1, 23,  &
     &   093, 01,'Cloud Height Indicator              ', 1.      , 1, 1, 24,  &
     &   094, 01,'Cloud Height                        ', 1.      , 2, 1, 25,  &
     &   095, 01,'Middle Cloud Type                   ', 1.      , 2, 1, 26,  &
     &   096, 01,'High Cloud Type                     ', 1.      , 2, 1, 27,  &
     &   098, 02,'Wave Direction                      ', 1.      , 1, 1, 15,  &
     &   099, 02,'Wave Period                         ', 1.      , 1, 1, 16,  &
     &   101, 02,'Wave Height                         ', 1.      , 1, 1, 17,  &
     &   103, 02,'Swell Direction                     ', 1.      , 1, 1, 18,  &
     &   105, 02,'Swell Period                        ', 1.      , 1, 1, 19,  &
     &   107, 02,'Swell Height                        ', 1.      , 1, 1, 20,  &
	 &   109, 02,'attm ID                             ', 1.      , 1, 0, 21,  &
	 &   111, 02,'attm length                         ', 1.      , 1, 0, 22,  &
	 &   113, 01,'box system indicator                ', 1.      , 1, 0, 23,  &
	 &   114, 03,'10 degree box number                ', 1.      , 1, 0, 24,  &
	 &   117, 01,'1 degree box number                 ', 1.      , 1, 0, 25,  &
	 &   118, 03,'deck                                ', 1.      , 1, 0, 26,  &
	 &   121, 03,'source ID                           ', 1.      , 1, 0, 27,  &
	 &   124, 02,'platform type                       ', 1.      , 1, 0, 28,  &
	 &   126, 02,'dup status                          ', 1.      , 1, 0, 29,  &
	 &   128, 01,'dup check                           ', 1.      , 1, 0, 30,  &
	 &   129, 01,'track check                         ', 1.      , 1, 0, 31,  &
	 &   130, 01,'pressure bias                       ', 1.      , 1, 0, 32,  &
	 &   131, 01,'wave period indicator               ', 1.      , 1, 0, 33,  &
	 &   132, 01,'swell period indicator              ', 1.      , 1, 0, 34,  &
	 &   133, 02,'2nd country code                    ', 1.      , 1, 1, 35/
      END