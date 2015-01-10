!==============================================================================!
!                         B E G I N    P R O G R A M :                         !
!                            H E L L O _ W O R L D                             !
!==============================================================================!
!                                                                              !
!   AUTHOR:         Christopher MacMackin                                      !
!   WRITTEN:        October, 2014                                              !
!   MODIFICATIONS:  None                                                       !
!                                                                              !
!   PURPOSE:        To demonstrate the structure of a basic Fortran program.   !
!                                                                              !
!   EXTERNALS:      None                                                       !
!                                                                              !
!------------------------------------------------------------------------------!
PROGRAM demo_program
    IMPLICIT NONE
    
    CHARACTER(LEN=15)   ::  message = "Hello, world!"
!------------------------------------------------------------------------------!

    WRITE(6,*) message
    message = "Bonjour, monde!"
    WRITE(6,*) message
    
    STOP
END PROGRAM demo_program    
!==============================================================================!
!                           E N D    P R O G R A M :                           !
!                            H E L L O _ W O R L D                             !
!==============================================================================!
