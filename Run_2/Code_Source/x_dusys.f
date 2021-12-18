C**********************************************************************
C
C     Copyright (C) 1992  Roland W. Freund and Noel M. Nachtigal
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
      subroutine dsys (A,IA,JA,RHS,LDOF,NEWMAT, NDIM,NZMAX,NZLMAX,
     +NZUMAX, JT,IDA,IL,IU,JI,JL,JU,AINV,DR,L,U,DN,DS,XZR, VECS)
 
 
 
 
 
C
C     This is an example driver code for the solution of linear systems
C     with coefficient  matrices stored  in the  compressed  sparse row
C     format.
C
C     The driver will ask the user for various run-time parameters, and
C     will expect to find the starting guess  in a file called `x.dat'.
C     Note that the  file name is in  lower case (for systems where the
C     case is significant, such as Unix).  All the vectors  are assumed
C     to be of length NROW.  It is  the responsibility  of the  user to
C     ensure that there is enough data in the files.
C
C     NOTE: This code uses an input format that, while valid on the Sun
C     and Cray compilers, might not be accepted  by other compilers. In
C     particular, the construct
C        WRITE (6,'(A30,$)') 'This is a test'
C     is used to prevent  the output processor from  moving to the next
C     line after writing the text.  If this construct  is not supported
C     by the compiler, remove the dollar sign ($).
C     Also, the user input in this code is not bullet-proof. It assumes
C     that the user will provide valid inputs, i.e., numbers where they
C     are expected, etc. Providing invalid input type will likely crash
C     the program.
C
C     Noel M. Nachtigal
C     March 9, 1992
C
C**********************************************************************
C
      EXTERNAL DUIL1I, DUIL1T, DUIL2I, DUIL2T, DUILST
      EXTERNAL DUS2SI, DUS2ST, DUSS1I, DUSS1T, DUSS2I, DUSS2T, DUSSST
      EXTERNAL DAXPBY, DUCPL, DUCPX, DUQBG, DUQMR, DUQMX, DUTFX
      EXTERNAL DSPAXB, DSPATX, DSPRAS, DSPRRW
C
C**********************************************************************
C
C     Parameters for the data format and the preconditioners.
 
C
 
c         PARAMETER(ndim=18000)
 
      INTEGER NDIM, NZMAX, NZLMAX, NZUMAX
 
C
C     Parameters for some of the solvers.
C
      INTEGER M, MAXN, MAXPQ, MAXVW
      PARAMETER (MAXN = 5000, MAXPQ = 3, MAXVW = 3)
      PARAMETER (M = MAXVW+MAXPQ+2)
C
C     Miscellaneous parameters.
C
      DOUBLE PRECISION DONE, DZERO
      PARAMETER (DONE = 1.0D0,DZERO = 0.0D0)
C
C     Variables for the data format and the preconditioners.
C
      LOGICAL JT(NDIM)
      CHARACTER TYPE*3
      INTEGER IA(NDIM+1), IDA(NDIM), IL(NDIM+1), IU(NDIM+1), JA(NZMAX)
      INTEGER JI(NDIM), JL(NZLMAX), JU(NZUMAX), NCOL, NROW, PRECON
      DOUBLE PRECISION A(NZMAX), AINV(NDIM), DR(NDIM)
      DOUBLE PRECISION L(NZLMAX), U(NZUMAX)
      DOUBLE PRECISION DN(NDIM), DS(NDIM), OMEGA
      DOUBLE PRECISION RHS(NDIM)
 
C
C
C     Variables common to all solvers.
C     All solvers use: NDIM, NLEN, NLIM, VECS, TOL, and INFO.
C
      INTEGER INFO(4), NLEN, NLIM
      DOUBLE PRECISION VECS(NDIM,5*(MAXPQ+MAXVW)+4), XZR(NDIM)
      DOUBLE PRECISION TOL
C
C     Variables specific to only some of the solvers.
C
      INTEGER IDX(6,MAXN), IWK(M,13), MVEC
      DOUBLE PRECISION DWK(M,MAXN+4), NORMS(2)
C
C     Variables used by reverse communication.
C
      INTEGER CB, CX, IERR, REVCOM
C
C     Local driver variables.
C
      INTEGER ALG, I, MAXXPQ, MAXXVW, OLDPRE, PRE
      LOGICAL NEWMAT
      CHARACTER ANS*1, FNAME*72
 
      DOUBLE PRECISION XTMP(NDIM)
 
      SAVE OLDPRE,PRE
 
 10   NROW = 0
 
 
C      CALL DSPRAS (NDIM,NZMAX,NROW,NCOL,A,IA,JA,TYPE,10,6,IERR,LDOF)
      CALL DSPRAS (NDIM,NZMAX,NROW,NCOL,A,IA,JA,TYPE,10,0,IERR,LDOF)
 
C
C     Check for errors.
C
      IF (IERR.NE.0) NROW = 0
 20   IF (NROW.EQ.0) THEN
         WRITE (6,'(A42)') 'Error encountered reading the matrix file.'
      ELSE IF (NROW.NE.NCOL) THEN
         WRITE (6,'(A28)') 'Error, matrix is not square.'
         NROW = 0
      END IF
      IF (NROW.EQ.0) GO TO 100
C
C     Read in the vector b.
C
 30   NLEN = 0
 
      NLEN = NROW
 
      DO 501 I = 1, NLEN
        	  VECS(I,2) = RHS(I)
  501 CONTINUE
 40   CONTINUE
 
 
 
C
C     Read in the vector x_0.
C
 50   NLEN = 0
      NLEN = NROW
      DO 601 I = 1, NLEN
        	  XZR(I) = 0.0 ! RHS(I)
  601 CONTINUE
 
 60   CONTINUE
 
 70   IF (NLEN.EQ.0) GO TO 100
C
C     Select an algorithm.
 
c      WRITE (6,'(A38)')  'Choices of algorithm         : 1 = CPL'
c      WRITE (6,'(A38)')  '                               2 = CPX'
c      WRITE (6,'(A38)')  '                               3 = QBG'
c      WRITE (6,'(A38)')  '                               4 = QMR'
c      WRITE (6,'(A38)')  '                               5 = QMX'
c      WRITE (6,'(A38)')  '                               6 = TFX'
c      WRITE (6,'(A31,$)')  'Select an algorithm          : '
 
 
      READ (5,*) ALG
 
      IF ((ALG.LT.1).OR.(ALG.GT.6)) GO TO 100
C
C     Get the convergence tolerance.
C
 
      READ (5,*) TOL
C
C     Get the maximum number of iteration steps.
 
 
      READ (5,*) NLIM
 
      IF (NLIM.GT.MAXN-2) THEN
         NLIM = MAXN-2
         WRITE (6,'(A31,I10)') 'NLIM too large, adjusted to  : ', NLIM
      END IF
C
C     Solver-specific initialization.
C
      IF (ALG.EQ.1) THEN
c         WRITE (6,'(A31,$)') 'Enter estimated norm for P&Q : '
         READ (5,*) NORMS(1)
c         WRITE (6,'(A31,$)') 'Enter estimated norm for V&W : '
         READ (5,*) NORMS(2)
 
 
C
C     Set the maximum block and storage information.
C
         MAXXPQ = MAXPQ
         MAXXVW = MAXVW
         MVEC   = MAXPQ + MAXVW
      ELSE IF (ALG.EQ.4) THEN
c         WRITE (6,'(A31,$)') 'Enter estimated matrix norm  : '
         READ (5,*) NORMS(1)
C
C     Set the maximum block and storage information.
C
         MAXXVW = MAXVW
      END IF
      IF (NLEN.EQ.0) GO TO 100
C
C     Select a preconditioner.
C
      OLDPRE = PRE
      PRE    = 0
 
      READ (5,*) PRECON
 
      IF ((PRECON.EQ.1).OR.(PRECON.EQ.2).OR.(PRECON.EQ.3)) THEN
         PRE = 1
      ELSE IF ((PRECON.EQ.4).OR.(PRECON.EQ.5).OR.(PRECON.EQ.6)) THEN
         PRE = 2
      ELSE
         PRECON = 0
         OLDPRE = PRE + 1
      END IF
C
C     Initialize the preconditioner.
C
      IF ((.NOT.NEWMAT).AND.(PRE.EQ.OLDPRE)) THEN
         READ (5,'(A1)') ANS
         NEWMAT = (ANS.EQ.'N').OR.(ANS.EQ.'n')
      END IF
      IF (NEWMAT.OR.(PRE.NE.OLDPRE)) THEN
         IF (PRE.EQ.1) THEN
            CALL DUILST (NROW,A,IA,JA,L,IL,JL,U,IU,JU,JT,IDA,DN,DR,DS,
     +      AINV,NZLMAX,NZUMAX,IERR)
         ELSE IF (PRE.EQ.2) THEN
            CALL DUSSST (NLEN,A,IA,JA,AINV,IDA,OMEGA,PRECON)
         END IF
      END IF
C
C     Compute the modified right hand side.
C
      CALL DSPAXB (NLEN,A,IA,JA,TYPE,XZR,XTMP)
      CALL DAXPBY (NLEN, VECS(1,2),DONE, VECS(1,2),-DONE,XTMP)
      IF (PRE.EQ.1) THEN
         CALL DUIL1I (NROW,L,IL,JL,U,IU,JU,AINV,PRECON, VECS(1,2))
      ELSE IF (PRE.EQ.2) THEN
         CALL DUSS1I (NLEN,A,IA,JA,AINV,IDA,OMEGA,PRECON, VECS(1,2))
      END IF
C
C     Set up call to linear systems solver.
C     Compute true residual norms, generate second starting vector.
C
      INFO(2) = 0
      INFO(1) = 010006
      INFO(1) = 000006
      INFO(1) = 000000


C
C
C     Open the output files.
C
C     OPEN (11,FILE = 'res.out')
C
C     Call the solver.
C
 80   IF (ALG.EQ.1) THEN
         CALL DUCPL (NDIM,NLEN,NLIM,MAXXPQ,MAXXVW,M,MVEC,NORMS,DWK, IDX,
     +   IWK,VECS,TOL,INFO)
      ELSE IF (ALG.EQ.2) THEN
         CALL DUCPX (NDIM,NLEN,NLIM,VECS,TOL,INFO)
      ELSE IF (ALG.EQ.3) THEN
         CALL DUQBG (NDIM,NLEN,NLIM,VECS,TOL,INFO)
      ELSE IF (ALG.EQ.4) THEN
         CALL DUQMR (NDIM,NLEN,NLIM,MAXXVW,M,NORMS(1),DWK,IDX,IWK, VECS,
     +   TOL,INFO)
      ELSE IF (ALG.EQ.5) THEN
         CALL DUQMX (NDIM,NLEN,NLIM,VECS,TOL,INFO)
      ELSE IF (ALG.EQ.6) THEN
         CALL DUTFX (NDIM,NLEN,NLIM,VECS,TOL,INFO)
      END IF
C
C     Perform matrix-vector multiplications when needed.
C
      IERR   = INFO(1)
      REVCOM = INFO(2)
      CX     = INFO(3)
      CB     = INFO(4)
C
C     Multiply VECS(1,CX) with the preconditioned matrix.
C
      IF (REVCOM.EQ.1) THEN
         IF (PRE.EQ.0) THEN
            CALL DSPAXB (NROW,A,IA,JA,TYPE,VECS(1,CX),VECS(1,CB))
         ELSE IF (PRE.EQ.1) THEN
            CALL DAXPBY (NLEN,XTMP,DONE,VECS(1,CX),DZERO,XTMP)
            CALL DUIL2I (NROW,L,IL,JL,U,IU,JU,AINV,PRECON,XTMP)
            CALL DSPAXB (NROW,A,IA,JA,TYPE,XTMP,VECS(1,CB))
            CALL DUIL1I (NROW,L,IL,JL,U,IU,JU,AINV,PRECON,VECS(1,CB))
         ELSE IF ((PRECON.EQ.4).OR.(PRECON.EQ.5)) THEN
            CALL DAXPBY (NLEN,XTMP,DONE,VECS(1,CX),DZERO,XTMP)
            CALL DUSS2I (NLEN,A,IA,JA,AINV,IDA,OMEGA,PRECON,XTMP)
            CALL DSPAXB (NROW,A,IA,JA,TYPE,XTMP,VECS(1,CB))
            CALL DUSS1I (NLEN,A,IA,JA,AINV,IDA,OMEGA,PRECON,VECS(1,CB))
         ELSE IF (PRECON.EQ.6) THEN
            CALL DAXPBY (NLEN,VECS(1,CB),DONE,VECS(1,CX),DZERO,XTMP)
            CALL DUS2SI (NROW,A,IA,JA,AINV,IDA,OMEGA,PRECON, VECS(1,CB),
     +      XTMP)
         END IF
         GO TO 80
C
C     Multiply VECS(1,CX) with the preconditioned transpose.
C
      ELSE IF (REVCOM.EQ.2) THEN
         IF (PRE.EQ.0) THEN
            CALL DSPATX (NLEN,A,IA,JA,TYPE,VECS(1,CX),VECS(1,CB))
         ELSE IF (PRE.EQ.1) THEN
            CALL DAXPBY (NLEN,XTMP,DONE,VECS(1,CX),DZERO,XTMP)
            CALL DUIL1T (NROW,L,IL,JL,U,IU,JU,AINV,PRECON,XTMP)
            CALL DSPATX (NLEN,A,IA,JA,TYPE,XTMP,VECS(1,CB))
            CALL DUIL2T (NROW,L,IL,JL,U,IU,JU,AINV,PRECON,VECS(1,CB))
         ELSE IF ((PRECON.EQ.4).OR.(PRECON.EQ.5)) THEN
            CALL DAXPBY (NLEN,XTMP,DONE,VECS(1,CX),DZERO,XTMP)
            CALL DUSS1T (NLEN,A,IA,JA,AINV,IDA,OMEGA,PRECON,XTMP)
            CALL DSPATX (NLEN,A,IA,JA,TYPE,XTMP,VECS(1,CB))
            CALL DUSS2T (NLEN,A,IA,JA,AINV,IDA,OMEGA,PRECON,VECS(1,CB))
         ELSE IF (PRECON.EQ.6) THEN
            CALL DAXPBY (NLEN,VECS(1,CB),DONE,VECS(1,CX),DZERO,XTMP)
            CALL DUS2ST (NROW,A,IA,JA,AINV,IDA,OMEGA,PRECON, VECS(1,CB),
     +      XTMP)
         END IF
         GO TO 80
      END IF
C
C     Close the output files.
C
 
      CLOSE (11)
 
C
C     Check why the solver stopped (this could be more compact).
C
      IF (IERR.EQ.0             ) THEN
         WRITE (6,'(A32)') 'The residual norm has converged.'
         GO TO 90
      ELSE IF (IERR.EQ.1             ) THEN
         WRITE (6,'(A35)') 'Invalid reverse communication call.'
         GO TO 90
      ELSE IF (IERR.EQ.2             ) THEN
         WRITE (6,'(A27)') 'Invalid inputs encountered.'
         GO TO 90
      ELSE IF (IERR.EQ.4             ) THEN
         WRITE (6,'(A31)') 'The algorithm did not converge.'
         GO TO 90
      END IF
      IF (ALG.EQ.1) THEN
         IF (IERR.LT.0) THEN
            WRITE (6,'(A40,I5)')
     +      'Error encountered in the DSVDC routine: ', -IERR
 
            GO TO 90
         ELSE IF (IERR.EQ.8             ) THEN
            WRITE (6,'(A35)') 'The last block could not be closed.'
            GO TO 90
         ELSE IF (IERR.EQ.16              ) THEN
            WRITE (6,'(A39)') 'An A-invariant subspace has been found.'
            GO TO 90
         ELSE IF (IERR.EQ.32              ) THEN
            WRITE (6,'(A41)')'An A^T-invariant subspace has been found.'
 
            GO TO 90
         ELSE IF (IERR.EQ.48            ) THEN
            WRITE (6,'(A41)')'Both invariant subspaces have been found.'
 
            GO TO 90
         ELSE IF (IERR.EQ.64            ) THEN
            WRITE (6,'(A30)') 'Insufficient memory allocated.'
            GO TO 90
         ELSE IF (IERR.EQ.128           ) THEN
            WRITE (6,'(A32)') 'Cannot convert regular to inner.'
            GO TO 90
         END IF
      ELSE IF (ALG.EQ.2) THEN
         IF (IERR.EQ.8              ) THEN
            WRITE (6,'(A25)') 'The algorithm broke down.'
            GO TO 90
         ELSE IF (IERR.EQ.16              ) THEN
            WRITE (6,'(A39)') 'An A-invariant subspace has been found.'
            GO TO 90
         ELSE IF (IERR.EQ.32              ) THEN
            WRITE (6,'(A41)')'An A^T-invariant subspace has been found.'
 
            GO TO 90
         ELSE IF (IERR.EQ.48            ) THEN
            WRITE (6,'(A41)')'Both invariant subspaces have been found.'
 
            GO TO 90
         END IF
      ELSE IF (ALG.EQ.3) THEN
         IF (IERR.EQ.8              ) THEN
            WRITE (6,'(A25)') 'The algorithm broke down.'
            GO TO 90
         END IF
      ELSE IF (ALG.EQ.4) THEN
         IF (IERR.LT.0) THEN
            WRITE (6,'(A40,I5)')
     +      'Error encountered in the DSVDC routine: ', -IERR
 
            GO TO 90
         ELSE IF (IERR.EQ.8             ) THEN
            WRITE (6,'(A35)') 'The last block could not be closed.'
            GO TO 90
         ELSE IF (IERR.EQ.16              ) THEN
            WRITE (6,'(A39)') 'An A-invariant subspace has been found.'
            GO TO 90
         ELSE IF (IERR.EQ.32              ) THEN
            WRITE (6,'(A41)')'An A^T-invariant subspace has been found.'
 
            GO TO 90
         ELSE IF (IERR.EQ.48            ) THEN
            WRITE (6,'(A41)')'Both invariant subspaces have been found.'
 
            GO TO 90
         END IF
      ELSE IF (ALG.EQ.5) THEN
         IF (IERR.EQ.8              ) THEN
            WRITE (6,'(A25)') 'The algorithm broke down.'
            GO TO 90
         ELSE IF (IERR.EQ.16              ) THEN
            WRITE (6,'(A39)') 'An A-invariant subspace has been found.'
            GO TO 90
         ELSE IF (IERR.EQ.32              ) THEN
            WRITE (6,'(A41)')'An A^T-invariant subspace has been found.'
 
            GO TO 90
         ELSE IF (IERR.EQ.48            ) THEN
            WRITE (6,'(A41)')'Both invariant subspaces have been found.'
 
            GO TO 90
         END IF
      ELSE IF (ALG.EQ.6) THEN
         IF (IERR.EQ.8              ) THEN
            WRITE (6,'(A25)') 'The algorithm broke down.'
            GO TO 90
         END IF
      END IF
      WRITE (6,'(A19,I5)') 'Unknown INFO code: ', IERR
C
C     Compute the unpreconditioned solution.
C
 90   IF (PRE.EQ.1) THEN
         CALL DUIL2I (NROW,L,IL,JL,U,IU,JU,AINV,PRECON,VECS(1,1))
      ELSE IF (PRE.EQ.2) THEN
         CALL DUSS2I (NLEN,A,IA,JA,AINV,IDA,OMEGA,PRECON,VECS(1,1))
      END IF
      CALL DAXPBY (NLEN,XZR,DONE,XZR,DONE,VECS(1,1))
 
      DO 801 I = 1, NLEN
        	  RHS(I) = XZR(I)
801   CONTINUE
 
C
C     Do it again?
C
 100  CONTINUE
 
 
      close(5)
 
      RETURN
      END
C
C**********************************************************************
 
