C**********************************************************************
C
C     Copyright (C) 1991-1992  Roland W. Freund and Noel M. Nachtigal
C     All rights reserved.
C
C     This code is part of a copyrighted package.  For details, see the
C     file `cpyrit.doc' in the top-level directory.
C
C     *****************************************************************
C     ANY USE OF  THIS CODE CONSTITUTES ACCEPTANCE OF  THE TERMS OF THE
C                             COPYRIGHT NOTICE
C     *****************************************************************
C
C**********************************************************************
C
C
C     This file contains support routines for sparse matrices stored in
C     the compressed sparse row (CSR) or compressed sparse column (CSC)
C     format.  The following routines are in this file:
C
C     SUBROUTINE DSPATX (NROW,A,IA,JA,TYPE,X,B)
C        Computes B = A^T * X.
C     SUBROUTINE DSPAXB (NROW,A,IA,JA,TYPE,X,B)
C        Computes B = A * X.
C     INTEGER FUNCTION DSPIND (I)
C        Computes the print field width for I.
C     SUBROUTINE DSPRAS (NDIM,NZMAX,NROW,NCOL,A,IA,JA,TYPE,INF,VF,IERR)
C        Reads in a CSR or CSC matrix from an ASCII data file.
C     SUBROUTINE DSPRRW (NDIM,NZMAX,NROW,NCOL,A,IA,JA,TYPE,INF,VF,IERR)
C        Reads in a CSR or CSC matrix from a raw data file.
C     SUBROUTINE DSPSRT (N,AROW,JROW)
C        Sorts the elements of row AROW in  order of  increasing column
C        index.
C     SUBROUTINE DSPTRS (NROW,NCOL,A,IA,JA,AT,IAT,JAT,POS,JOB)
C        Transposes a CSR or CSC matrix.
C     SUBROUTINE DSPU2S (NROW,A,IA,JA,DIAG)
C        Converts a symmetric matrix from CSR format to modified CSR
C        format (diagonal stored first).
C     SUBROUTINE DSPWAS (NROW,NCOL,A,IA,JA,TITLE,KEY,TYPE,IDIG,OUTF,
C        IERR)
C        Prints out the matrix A to an ASCII file.
C
C**********************************************************************
C
      SUBROUTINE DSPATX (NROW,A,IA,JA,TYPE,X,B)
C
C     Purpose:
C     This subroutine computes B = A^T * X, for a sparse matrix A.
C
C     Parameters:
C     NROW = the number of rows in the matrix (input).
C     A    = the matrix elements (input).
C     IA   = the array of row pointers (input).
C     JA   = the array of column indices (input).
C     TYPE = the matrix type (input).
C     X    = the vector to be multiplied by A^T (input).
C     B    = the result of the multiplication (output).
C
C     External routines used:
C     subroutine dspaxb (nrow,a,ia,ja,type,x,b)
C        Computes b = A * x, called for symmetric matrices.
C
C     Noel M. Nachtigal
C     September 27, 1990
C
C**********************************************************************
C
      EXTERNAL DSPAXB
C
      CHARACTER TYPE*3
      INTEGER JA(*), NROW, IA(NROW+1)
      DOUBLE PRECISION A(*), B(NROW), X(NROW)
C
C     Miscellaneous parameters.
C
      DOUBLE PRECISION DZERO
      PARAMETER (DZERO = 0.0D0)
C
C     Local variables.
C
      INTEGER I, J, K
C
C     Multiply by A^T.
C
      IF (TYPE(2:2).EQ.'U') THEN
         DO 10 I = 1, NROW
            B(I) = DZERO
 10      CONTINUE
         DO 30 I = 1, NROW
            DO 20 K = IA(I), IA(I+1)-1
               J    = JA(K)
               B(J) = B(J) + A(K) * X(I)
 20         CONTINUE
 30      CONTINUE
      ELSE IF (TYPE(2:2).EQ.'S') THEN
         CALL DSPAXB (NROW,A,IA,JA,TYPE,X,B)
      END IF
C
      RETURN
      END
C
C**********************************************************************
C
      SUBROUTINE DSPAXB (NROW,A,IA,JA,TYPE,X,B)
C
C     Purpose:
C     This subroutine computes B = A * X, for a sparse matrix A.
C
C     Parameters:
C     NROW = the number of rows in the matrix (input).
C     A    = the matrix elements (input).
C     IA   = the array of row pointers (input).
C     JA   = the array of column indices (input).
C     TYPE = the matrix type (input).
C     X    = the vector to be multiplied by A (input).
C     B    = the result of the multiplication (output).
C
C     External routines used:
C     None.
C
C     Noel M. Nachtigal
C     September 27, 1990
C
C**********************************************************************
C
      CHARACTER TYPE*3
      INTEGER JA(*), NROW, IA(NROW+1)
      DOUBLE PRECISION A(*), B(NROW), X(NROW)
C
C     Miscellaneous parameters.
C
      DOUBLE PRECISION DZERO
      PARAMETER (DZERO = 0.0D0)
C
C     Local variables.
C
      INTEGER I, J, K
C
C     Multiply by A.
C
      IF (TYPE(2:2).EQ.'U') THEN
         DO 20 I = 1, NROW
            B(I) = DZERO
            DO 10 K = IA(I), IA(I+1)-1
               B(I) = B(I) + A(K) * X(JA(K))
 10         CONTINUE
 20      CONTINUE
      ELSE IF (TYPE(2:2).EQ.'S') THEN
         DO 30 I = 1, NROW
            B(I) = DZERO
 30      CONTINUE
         DO 50 I = 1, NROW
            B(I) = B(I) + A(I) * X(I)
            DO 40 K = IA(I), IA(I+1)-1
               J    = JA(K)
               B(I) = B(I) + X(J) * A(K)
               B(J) = B(J) + X(I) * A(K)
 40         CONTINUE
 50      CONTINUE
      END IF
C
      RETURN
      END
C
C**********************************************************************
C
      INTEGER FUNCTION DSPIND (I)
C
C     Purpose:
C     This function returns the minimum width of the printing field for
C     I.  This includes the sign field, if needed.
C
C     Parameters:
C     I = the integer to be printed (input).
C
C     Noel M. Nachtigal
C     October 20,1993
C
C**********************************************************************
C
      INTEGER I
C
C     Local variables.
C
      INTEGER ITMP, IWID
C
C     Extract the sign, if present.
C
      IF (I.GE.0) THEN
         ITMP = I
         IWID = 1
      ELSE
         IWID = 2
         ITMP = -I
      END IF
C
C     Compute the width of the remaining positive integer.
C
 10   IF (ITMP.LT.10) GO TO 20
      ITMP = ITMP / 10
      IWID = IWID + 1
      GO TO 10
C
 20   DSPIND = IWID
C
      RETURN
      END
C
C**********************************************************************
C
      SUBROUTINE DSPRAS (NDIM,NZMAX,NROW,NCOL,A,IA,JA,TYPE,INF,VF,IERR
     .                                                         ,LDOF)
C
C     Purpose:
C     This subroutine reads in a compressed sparse matrix from the file
C     INF, an ASCII file containing the matrix  stored according to the
C     Harwell-Boeing format.  Note that the format does not distinguish
C     between the compressed sparse  column format (Harwell-Boeing) and
C     the compressed sparse row format (SPARSKIT).
C
C     Parameters:
C     NDIM  = maximum number of rows and columns in the matrix; must be
C             at least 1 (input).
C     NZMAX = maximum number of nonzero elements in the matrix; must be
C             at least 1 (input).
C     NROW  = the number of rows in the matrix (output).
C     NCOL  = the numer of columns in the matrix (output).
C     A     = double precision array dimensioned at least NZMAX; filled
C             with the nonzero elements of the matrix (output).
C     IA    = integer array, dimensioned  at least NDIM+1;  filled with
C             pointers to locations  in the arrays  A and JA  where the
C             rows or columns start (output).
C     JA    = integer array, dimensioned  at least NZMAX;  filled  with
C             the column or row indices of nonzero elements (output).
C     TYPE  = the matrix type (output).
C     INF   = the input unit number,  assumed to be  open and ready for
C             input (input).
C     VF    = unit number for verbose messages.  If nonzero,  then  the
C             routine will output matrix stats to this unit (input).
C     IERR  = error code.  On output, it may be:
C                  0 = no errors;
C                  1 = matrix has too many nonzero elements;
C                  2 = matrix has too many columns;
C                  4 = matrix has too many rows;
C                  8 = matrix is not REAL (TYPE(1) must be 'R');
C                 16 = matrix is not assembled (TYPE(3) must be 'A');
C                128 = one of the READ operations ran into an error.
C              The error codes are additive;  for instance, IERR = 3 if
C              the matrix  has both  too many nonzero elements  and too
C              many columns (output).
C
C     Noel M. Nachtigal
C     June 14, 1993
C
C**********************************************************************
C
      
      INTEGER IERR, INF, NCOL, NROW,  VF
      INTEGER IA(NDIM+1), JA(NZMAX)
      DOUBLE PRECISION A(NZMAX)
      CHARACTER TYPE*3
C
C     Local variables.
C
      CHARACTER FMTIND*16, FMTPTR*16, FMTVAL*20, FMTRHS*20
      CHARACTER KEY*8, RHSTYP*3, TITLE*72
      INTEGER I, LINTOT, LINPTR, LININD, LINVAL, LINRHS, LINROW
      INTEGER NELMTL, NNZ, NRHS
C
C     Clear the error flag and initialize variables.
C
      IERR = 0
      NRHS = 0
      LINROW = 0

      NROW   = LDOF
      NCOL   = LDOF
      NNZ    = IA(NROW+1) - 1
      JOB    = 2
      NRHS   = 0
      IOUNIT = 10
      TITLE  ='sparse.dat'
C      GUESOL ='No'
      KEY = 'RUA'
      TYPE = 'RUA'
C
C Read in the matrix header data.
C
cIIIII      READ (INF,'(A72,A8)',END=10,ERR=10) TITLE, KEY
cIIIII      READ (INF,'(5I14)',END=10,ERR=10)
cIIIII     $      LINTOT, LINPTR, LININD, LINVAL, LINRHS
cIIIII      READ (INF,'(A3,11X,4I14)',END=10,ERR=10) TYPE, NROW, NCOL, NNZ, 
cIIIII     $NELMTL
cIIIII      READ (INF,'(2A16,2A20)',END=10,ERR=10) FMTPTR, FMTIND, FMTVAL, 
cIIIII     $FMTRHS
cIIIII      IF (LINRHS.GT.0) 
cIIIII     $    READ (INF,'(A3,11X,2I14)',END=10,ERR=10) RHSTYP, NRHS, LINROW
C
C     Output the matrix parameters.
C      
      IF (VF.NE.0) THEN
         WRITE (VF,'(A7,A73)') 'TITLE :',TITLE
         WRITE (VF,'(A7,A11)') 'KEY   :',KEY
         WRITE (VF,'(A7,A10)') 'TYPE  :',TYPE
         WRITE (VF,'(A7,I10)') 'NDIM  :',NDIM
         WRITE (VF,'(A7,I10)') 'NROW  :',NROW
         WRITE (VF,'(A7,I10)') 'NCOL  :',NCOL
         WRITE (VF,'(A7,I10)') 'NZMAX :',NZMAX
         WRITE (VF,'(A7,I10)') 'NNZ   :',NNZ
         WRITE (VF,'(A7,I10)') 'NRHS  :',NRHS
      END IF
C
C     Check for errors.
C
      IF (NNZ.GT.NZMAX) IERR = IERR + 1
      IF (NCOL.GT.NDIM) IERR = IERR + 2
      IF (NROW.GT.NDIM) IERR = IERR + 4
      IF (TYPE(1:1).NE.'R') IERR = IERR + 8
      IF (TYPE(3:3).NE.'A') IERR = IERR + 16
      IF (IERR.NE.0) RETURN
C
C     Read in the matrix data.
C
cIIIII      READ (INF,FMTPTR,END=10,ERR=10) (IA(I),I=1,NROW+1)
cIIIII      READ (INF,FMTIND,END=10,ERR=10) (JA(I),I=1,NNZ)
cIIIII      READ (INF,FMTVAL,END=10,ERR=10) (A(I),I=1,NNZ)
C
C     Close the file.
C
cIIIII      CLOSE (INF)
C
      RETURN 
C
C     Handle any read errors.
C
 10   IERR = IERR + 128
      RETURN
C
      END
C
C**********************************************************************
C
      SUBROUTINE DSPRRW (NDIM,NZMAX,NROW,NCOL,A,IA,JA,TYPE,INF,VF,IERR)
C
C     Purpose:
C     This subroutine reads in a compressed sparse matrix from the file
C     INF, a binary file containing the matrix  stored according to the
C     Harwell-Boeing format.  Note that the format does not distinguish
C     between the compressed sparse  column format (Harwell-Boeing) and
C     the compressed sparse row format (SPARSKIT).
C
C     Parameters:
C     NDIM  = maximum number of rows and columns in the matrix; must be
C             at least 1 (input).
C     NZMAX = maximum number of nonzero elements in the matrix; must be
C             at least 1 (input).
C     NROW  = the number of rows in the matrix (output).
C     NCOL  = the number of columns in the matrix (output).
C     A     = double precision array dimensioned at least NZMAX; filled
C             with the nonzero elements of the matrix (output).
C     IA    = integer array, dimensioned  at least NDIM+1;  filled with
C             pointers to locations  in the arrays  A and JA  where the
C             rows or columns start (output).
C     JA    = integer array, dimensioned  at least NZMAX;  filled  with
C             the column or row indices of nonzero elements (output).
C     TYPE  = the matrix type (output).
C     INF   = the input unit number, assumed to be  open in unformatted
C             mode and ready for input (input).
C     VF    = unit number for verbose messages.  If nonzero,  then  the
C             routine will output matrix stats to this unit (input).
C     IERR  = error code.  On output, it may be:
C                  0 = no errors;
C                  1 = matrix has too many nonzero elements;
C                  2 = matrix has too many columns;
C                  4 = matrix has too many rows;
C                  8 = matrix is not REAL (TYPE(1) must be 'R');
C                 16 = matrix is not assembled (TYPE(3) must be 'A');
C                128 = one of the READ operations ran into an error.
C              The error codes are additive;  for instance, IERR = 3 if
C              the matrix  has both  too many nonzero elements  and too
C              many columns (output).
C
C     Noel M. Nachtigal
C     June 14, 1993
C
C**********************************************************************
C
      INTEGER IERR, INF, NCOL, NROW,  VF
      INTEGER IA(NDIM+1), JA(NZMAX)
      DOUBLE PRECISION A(NZMAX)
      CHARACTER TYPE*3
C
C     Magic numbers.
C
      INTEGER ZMGINT
      CHARACTER*32 ZMGCHR
      DOUBLE PRECISION DMGNUM
      PARAMETER (ZMGINT=38401318)
      PARAMETER (ZMGCHR='0123456789ABCDEFFEDCBA9876543210')
      PARAMETER (DMGNUM=2.508780732563974D-04)
C
C     Checkpoint variables.
C
      INTEGER CHKINT
      CHARACTER*32 CHKCHR
      DOUBLE PRECISION CHKNUM
C
C     Local variables.
C
      CHARACTER FMTIND*16, FMTPTR*16, FMTVAL*20, FMTRHS*20
      CHARACTER KEY*8, RHSTYP*3, TITLE*72
      INTEGER I, LINTOT, LINPTR, LININD, LINVAL, LINRHS, LINROW
      INTEGER NELMTL, NNZ, NRHS
C
C     Clear the error flag.
C
      IERR = 0
C
C     Read in the magic numbers.
C
      READ (INF,END=10,ERR=10) CHKCHR, CHKNUM, CHKINT
      IF ((CHKCHR.NE.ZMGCHR).OR.(CHKNUM.NE.DMGNUM).OR.
     $    (CHKINT.NE.ZMGINT)) THEN
         WRITE (6,'(A30)') 'Unrecognized binary data file.'
         CLOSE (INF)
         GO TO 10
      END IF
C
C     Read in the matrix header data.
C
      READ (INF,END=10,ERR=10) TITLE, KEY
      READ (INF,END=10,ERR=10) LINTOT, LINPTR, LININD, LINVAL, LINRHS
      READ (INF,END=10,ERR=10) TYPE, NROW, NCOL, NNZ, NELMTL
      READ (INF,END=10,ERR=10) FMTPTR, FMTIND, FMTVAL, FMTRHS
      IF (LINRHS.GT.0) READ (INF,END=10,ERR=10) RHSTYP, NRHS, LINROW
C
C     Output the matrix parameters.
C
      IF (VF.NE.0) THEN
         WRITE (VF,'(A7,A73)') 'TITLE :',TITLE
         WRITE (VF,'(A7,A11)') 'KEY   :',KEY
         WRITE (VF,'(A7,A10)') 'TYPE  :',TYPE
         WRITE (VF,'(A7,I10)') 'NDIM  :',NDIM
         WRITE (VF,'(A7,I10)') 'NROW  :',NROW
         WRITE (VF,'(A7,I10)') 'NCOL  :',NCOL
         WRITE (VF,'(A7,I10)') 'NZMAX :',NZMAX
         WRITE (VF,'(A7,I10)') 'NNZ   :',NNZ
         WRITE (VF,'(A7,I10)') 'NRHS  :',NRHS
      END IF
C
C     Check for errors.
C
      IF (NNZ.GT.NZMAX) IERR = IERR + 1
      IF (NCOL.GE.NDIM) IERR = IERR + 2
      IF (NROW.GE.NDIM) IERR = IERR + 4
      IF (TYPE(1:1).NE.'R') IERR = IERR + 8
      IF (TYPE(3:3).NE.'A') IERR = IERR + 16
      IF (IERR.NE.0) RETURN
C
C     Read in the matrix data.
C
      READ (INF,END=10,ERR=10) (IA(I),I=1,NROW+1)
      READ (INF,END=10,ERR=10) (JA(I),I=1,NNZ)
      READ (INF,END=10,ERR=10) (A(I),I=1,NNZ)
C
C     Close the file.
C
      CLOSE (INF)
C
      RETURN 
C
C     Handle any read errors.
C
 10   IERR = IERR + 128
      RETURN
C
      END
C
C**********************************************************************
C
      SUBROUTINE DSPSRT (N,AROW,JROW)
C
C     Purpose:
C     This subroutine sorts  the column indices  of a  row of  a matrix
C     stored in compressed sparse row (CSR) format in increasing order.
C     The routine  is given the  array of elements of  the row in AROW,
C     with the corresponding  column indices  in JROW.  Both arrays are
C     of length N; the elements are given so  that they can  be kept in
C     correspondence with the elements in AROW.
C
C     Parameters:
C     N    = the length of the row (input).
C     AROW = the row to be sorted (input/output).
C     JROW = the array of matching column indices (input/output).
C
C     Noel M. Nachtigal
C     October 28, 1990
C
C**********************************************************************
C
      INTEGER N, JROW(N)
      DOUBLE PRECISION AROW(N)
C
C     Local variables.
C
      INTEGER I, J, JTMP, K, L
      DOUBLE PRECISION DTMP
C
      IF (N.LE.1) RETURN
C
      L = N / 2 + 1
      K = N
 10   IF (L.GT.1) THEN
         L    = L - 1
         DTMP = AROW(L)
         JTMP = JROW(L)
      ELSE
         DTMP    = AROW(K)
         JTMP    = JROW(K)
         AROW(K) = AROW(1)
         JROW(K) = JROW(1)
         K       = K - 1
         IF (K.LE.1) THEN
            AROW(1) = DTMP
            JROW(1) = JTMP
            RETURN
         END IF
      END IF
      I = L
      J = L + L
 20   IF (J.LE.K) THEN
         IF (J.LT.K) THEN
            IF (JROW(J).LT.JROW(J+1)) J = J + 1
         END IF
         IF (JTMP.LT.JROW(J)) THEN
            AROW(I) = AROW(J)
            JROW(I) = JROW(J)
            I       = J
            J       = J + J
         ELSE
            J = K + 1
         END IF
         GO TO 20
      END IF
      AROW(I) = DTMP
      JROW(I) = JTMP
      GO TO 10
C
      END
C
C**********************************************************************
C
      SUBROUTINE DSPTRS (NROW,NCOL,A,IA,JA,AT,IAT,JAT,POS,JOB)
C
C     Purpose:
C     This subroutine transposes  a matrix stored in  compressed sparse
C     row (CSR). In addition to the transposed pointers in IAT and JAT,
C     the routine will return either the transposed values array in AT,
C     or pointers for the transposed values array in POS.  The relation
C     between the two is AT(K) = A(POS(K)).
C
C     Parameters:
C     NROW = the row dimension of the input matrix (input).
C     NCOL = the column dimension of the input matrix (input).
C     A    = the elements of the input matrix, if the  transposed array
C            AT is desired (input).
C     IA   = the array of row pointers for A (input).
C     JA   = the array of column indices for A (input).
C     AT   = the elements of the transpose matrix, optional (output).
C     IAT  = the array of row pointers for AT (output).
C     JAT  = the array of column indices for AT (output).
C     POS  = the array of positions in A corresponding to the AT array,
C            AT(K) = A(POS(K)), optional (output).
C     JOB  = the requested operation; if JOB = 0, the AT is filled with
C            the elements of the transpose and POS is not used  at all,
C            otherwise, POS is filled with the pointers to A, and AT is
C            not used at all (input).
C
C     Noel M. Nachtigal
C     May 28, 1992
C
C**********************************************************************
C
      INTEGER JOB, NCOL, NROW
      INTEGER IA(NROW+1), IAT(NCOL+1), JA(*), JAT(*), POS(*)
      DOUBLE PRECISION A(*), AT(*)
C
C     Local variables.
C
      INTEGER I, J, K, NEXT
C
C     Initialize the pointers.
C
      DO 10 I = 1, NCOL+1
         IAT(I) = 0
 10   CONTINUE
C
C     Find out how many elements are in each column.
C     The counts are shifted up one position.
C
      DO 20 K = IA(1), IA(NROW+1)-1
         IAT(JA(K)+1) = IAT(JA(K)+1) + 1
 20   CONTINUE
C
C     Fixup the new pointers.
C
      IAT(1) = 1
      DO 30 I = 2, NCOL+1
         IAT(I) = IAT(I) + IAT(I-1)
 30   CONTINUE
C
C     Now transpose the matrix.
C     The new row pointers are again shifted up one position.
C
      DO 50 I = 1, NROW
         DO 40 K = IA(I), IA(I+1)-1
            J = JA(K)
            NEXT = IAT(J)
            IF (JOB.EQ.0) THEN
               AT(NEXT) = A(K)
            ELSE
               POS(NEXT) = K
            END IF
            JAT(NEXT) = I
            IAT(J) = NEXT+1
 40      CONTINUE
 50   CONTINUE
C
C     Fix up the new row pointers.
C
      DO 60 I = NCOL+1, 2, -1
         IAT(I) = IAT(I-1)
 60   CONTINUE
      IAT(1) = 1
C
      RETURN
      END
C
C**********************************************************************
C
      SUBROUTINE DSPU2S (NROW,A,IA,JA,DIAG)
C
C     Purpose:
C     This subroutine converts the CSR matrix A to modified CSR format,
C     where the diagonal is stored first.  It also eliminates the upper
C     triangle, thus it is suitable only for symmetric matrices.
C
C     Parameters:
C     NROW  = the row dimension of the input matrix (input).
C     A     = the elements of the matrix (input/output).
C     IA    = the array of row pointers for A (input/output).
C     JA    = the array of column indices for A (input/output).
C     DIAG  = temporary array for the diagonal elements (output).
C
C     Noel M. Nachtigal
C     February 24, 1994
C
C**********************************************************************
C
      INTEGER NROW, IA(NROW+1), JA(*)
      DOUBLE PRECISION A(*), DIAG(NROW)
C
C     Miscellaneous parameters.
C
      DOUBLE PRECISION DZERO
      PARAMETER (DZERO = 0.0D0)
C
C     Local variables.
C
      INTEGER I, IAIP1, J, K, LASTK, NEXTK
C
C     Extract the diagonal elements and the lower triangle.
C
      IAIP1 = IA(NROW+1)
      LASTK = IAIP1-1
      DO 20 I = NROW, 1, -1
         DIAG(I) = DZERO
         DO 10 K = IAIP1-1, IA(I), -1
            J = JA(K)
            IF (I.GT.J) THEN
               A(LASTK)  = A(K)
               JA(LASTK) = J
               LASTK     = LASTK - 1
            ELSE IF (I.EQ.J) THEN
               DIAG(I) = A(K)
            END IF
 10      CONTINUE
         IAIP1 = IA(I)
         IA(I) = LASTK+1
 20   CONTINUE
C
C     Reorder the arrays A, IA and JA.
C
      IF (LASTK.NE.NROW) THEN
         NEXTK = NROW + 1
         DO 30 K = LASTK+1, IA(NROW+1)-1
            A(NEXTK)  = A(K)
            JA(NEXTK) = JA(K)
            NEXTK     = NEXTK + 1
 30      CONTINUE
      END IF
      DO 40 I = 1, NROW
         A(I)  = DIAG(I)
         JA(I) = I
         IA(I) = IA(I) - LASTK + NROW
 40   CONTINUE
      IA(NROW+1) = IA(NROW+1) - LASTK + NROW
      RETURN
      END
C
C**********************************************************************
C
      SUBROUTINE DSPWAS (NROW,NCOL,A,IA,JA,TITLE,KEY,TYPE,IDIG,OUTF,
     $                   IERR)
C
C     Purpose:
C     This subroutine writes out an ASCII file for a compressed sparse
C     row matrix.
C
C     Parameters:
C     NROW  = the row dimension of the input matrix (input).
C     NCOL  = the column dimension of the input matrix (input).
C     A     = the elements of the matrix (input).
C     IA    = the array of row pointers for A (input).
C     JA    = the array of column indices for A (input).
C     TITLE = the title of the matrix (input).
C     KEY   = the key for the matrix (input).
C     TYPE  = the type for the matrix (input).
C     IDIG  = the number of decimal digits to print, IDIG >= 1 (input).
C     OUTF  = unit number for the output file, must be open (input).
C     IERR  = error reporting variable, an error was encountered of the
C             variable is non-zero on exit (output).
C
C     Noel M. Nachtigal
C     October 20, 1993
C
C**********************************************************************
C
      INTRINSIC MAX0, MIN0
      EXTERNAL DSPIND
      INTEGER DSPIND
C
      INTEGER IDIG, IERR, JA(*), NCOL, NROW, OUTF, IA(NROW+1)
      CHARACTER KEY*8, TITLE*72, TYPE*3
      DOUBLE PRECISION A(*)
C
C     Local variables.
C
      INTEGER I, ICNT, IWID, LINLEN, LINTOT, LINPTR, LININD, LINVAL
      INTEGER LINRHS, LINROW, NELMTL, NNZ, NRHS
      CHARACTER FMTIND*16, FMTPTR*16, FMTVAL*20, FMTRHS*20, RHSTYP*3
C
C     Check the matrix dimensions.
C
      IERR = 0
      IF (NROW.LE.0) IERR = 1
      IF (NCOL.LE.0) IERR = 1
      IF (IERR.NE.0) GO TO 10
      NNZ = IA(NROW+1) - 1
      IF (NNZ.LE.0) THEN
         IERR = 1
         RETURN
      END IF
C
C     Initialize the line length, assume LINLEN.LE.99.
C
      LINLEN = 80
C
C     Compute the field width for NNZ+1, this determines FMTPTR and
C     LINPTR for IA.  There are NROW+1 pointers in IA, each one in the
C     range from 1 to NNZ+1.
C
      IWID = DSPIND(NNZ+1)
      ICNT = LINLEN / IWID
      IF (ICNT.LT.1) THEN
         IERR = 2
         RETURN
      END IF
C
C     Here: 1.LE.ICNT.LE.LINLEN, and 1.LE.IWID.LE.LINLEN.
C     Assume that LINLEN.LE.99.
C
      ICNT   = MIN0(ICNT,NROW+1)
      FMTPTR = '                '
      IF (ICNT.LT.10) THEN
         IF (IWID.LT.10) THEN
            WRITE (FMTPTR,'(A1,I1,A1,I1,A1)') '(', ICNT, 'I', IWID, ')'
         ELSE
            WRITE (FMTPTR,'(A1,I1,A1,I2,A1)') '(', ICNT, 'I', IWID, ')'
         END IF
      ELSE
         IF (IWID.LT.10) THEN
            WRITE (FMTPTR,'(A1,I2,A1,I1,A1)') '(', ICNT, 'I', IWID, ')'
         ELSE
            WRITE (FMTPTR,'(A1,I2,A1,I2,A1)') '(', ICNT, 'I', IWID, ')'
         END IF
      END IF
      LINPTR = (NROW+1+ICNT-1) / ICNT
C
C     Compute the field width for NCOL, this determines FMTIND and
C     LININD for JA.  There are NNZ pointers in JA, each one in the
C     range from 1 to NCOL.
C
      IWID = DSPIND(NCOL)
      ICNT = LINLEN / IWID
      IF (ICNT.LT.1) THEN
         IERR = 2
         RETURN
      END IF
C
C     Here: 1.LE.ICNT.LE.LINLEN, and 1.LE.IWID.LE.LINLEN.
C     Assume that LINLEN.LE.99.
C
      ICNT   = MIN0(ICNT,NNZ)
      FMTIND = '                '
      IF (ICNT.LT.10) THEN
         IF (IWID.LT.10) THEN
            WRITE (FMTIND,'(A1,I1,A1,I1,A1)') '(', ICNT, 'I', IWID, ')'
         ELSE
            WRITE (FMTIND,'(A1,I1,A1,I2,A1)') '(', ICNT, 'I', IWID, ')'
         END IF
      ELSE
         IF (IWID.LT.10) THEN
            WRITE (FMTIND,'(A1,I2,A1,I1,A1)') '(', ICNT, 'I', IWID, ')'
         ELSE
            WRITE (FMTIND,'(A1,I2,A1,I2,A1)') '(', ICNT, 'I', IWID, ')'
         END IF
      END IF
      LININD = (NNZ+ICNT-1) / ICNT
C
C     Compute the field width for the matrix values, this determines
C     FMTVAL and LINVAL for A.
C
      IDIG = MAX0(IDIG,1)
      IWID = IDIG + 6
      ICNT = LINLEN / IWID
      IF (ICNT.LT.1) THEN
         IERR = 2
         RETURN
      END IF
C
C     Here: 1.LE.ICNT.LE.LINLEN/6, and 6.LE.IWID.LE.LINLEN.
C     Assume that LINLEN.LE.99.
C
      ICNT   = MIN0(ICNT,NNZ)
      FMTVAL = '                    '
      IF (ICNT.LT.10) THEN
         IF (IDIG.LT.4) THEN
            WRITE (FMTVAL,'(A1,I1,A1,I1,A1,I1,A1)')
     $             '(', ICNT, 'D', IDIG+6, '.', IDIG, ')'
         ELSE IF (IDIG.LT.10) THEN
            WRITE (FMTVAL,'(A1,I1,A1,I2,A1,I1,A1)')
     $             '(', ICNT, 'D', IDIG+6, '.', IDIG, ')'
         ELSE
            WRITE (FMTVAL,'(A1,I1,A1,I2,A1,I2,A1)')
     $             '(', ICNT, 'D', IDIG+6, '.', IDIG, ')'
         END IF
      ELSE
         IF (IDIG.LT.4) THEN
            WRITE (FMTVAL,'(A1,I2,A1,I1,A1,I1,A1)')
     $             '(', ICNT, 'D', IDIG+6, '.', IDIG, ')'
         ELSE IF (IDIG.LT.10) THEN
            WRITE (FMTVAL,'(A1,I2,A1,I2,A1,I1,A1)')
     $             '(', ICNT, 'D', IDIG+6, '.', IDIG, ')'
         ELSE
            WRITE (FMTVAL,'(A1,I2,A1,I2,A1,I2,A1)')
     $             '(', ICNT, 'D', IDIG+6, '.', IDIG, ')'
         END IF
      END IF
      LINVAL = (NNZ+ICNT-1) / ICNT
C
C
C     These are defaults for things not handled right now.
C
      NRHS = 0
      LINRHS = 0
      LINROW = 0
      NELMTL = 0
      RHSTYP = '   '
      FMTRHS = '                    '
C
C     Compute the total number of lines.
C
      LINTOT = LINPTR + LININD + LINVAL + LINRHS
C
C     Write out the matrix.
C
      WRITE (OUTF,'(A72,A8)',ERR=10) TITLE, KEY
      WRITE (OUTF,'(5I14)',ERR=10)
     $       LINTOT, LINPTR, LININD, LINVAL, LINRHS
      WRITE (OUTF,'(A3,11X,4I14)',ERR=10)
     $       TYPE, NROW, NCOL, NNZ, NELMTL
      WRITE (OUTF,'(2A16,2A20)',ERR=10)
     $       FMTPTR, FMTIND, FMTVAL, FMTRHS
      IF (LINRHS.GT.0)
     $    WRITE (OUTF,'(A3,11X,2I14)',ERR=10) RHSTYP, NRHS, LINROW
      WRITE (OUTF,FMTPTR,ERR=10) (IA(I),I=1,NROW+1)
      WRITE (OUTF,FMTIND,ERR=10) (JA(I),I=1,NNZ)
      WRITE (OUTF,FMTVAL,ERR=10) (A(I),I=1,NNZ)
C
      RETURN
C
C     Handle any write errors.
C
 10   IERR = 3
      RETURN
      END
C
C**********************************************************************
