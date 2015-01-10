!==============================================================================!
!                          B E G I N    M O D U L E :                          !
!                                  T O O L S                                   !
!==============================================================================!
!                                                                              !
!   PURPOSE:    A module containing tools for the potential_field program used !
!               in my Fortran tutorial. In reality these two procedures are    !
!               sufficiently different that you would probably want to put     !
!               them in separate modules containing other, more similar tools: !
!               for example, one involving statistics and another involving    !
!               calculus. Unless you have a highly specific procedure which    !
!               could only be used in your current program, you should place   !
!               procedures in modules so that they can easily be reused for    !
!               future programs.                                               !
!                                                                              !
!   CONTAINS:   differentiate (function), stats (subroutine)                   !
!   EXTERNALS:  None                                                           !
!                                                                              !
!------------------------------------------------------------------------------!
MODULE tools
CONTAINS
    !==========================================================================!
    !                      B E G I N    F U N C T I O N :                      !
    !                         D I F F E R E N T I A T E                        !
    !==========================================================================!
    !                                                                          !
    !   AUTHOR:         Christopher MacMackin                                  !
    !   WRITTEN:        July, 2014                                             !
    !   MODIFICATIONS:  None                                                   !
    !                                                                          !
    !   PURPOSE:        Estimates the first derivative of some discrete data.  !
    !                   Returns a REAL(8) array of the same size as the input  !
    !                   arrays.                                                !
    !                                                                          !
    !   ARGUMENTS:     *independent, the independent data with respect to      !
    !                       which the differentiation will be performed.       !
    !                  *dependent, a rank-1 REAL(8) array containing the       !
    !                       independent data, whose derivative will be taken   !
    !                       with respect to the dependent data.                !
    !                                                                          !
    !   EXTERNALS:      None                                                   !
    !                                                                          !
    !--------------------------------------------------------------------------!
    FUNCTION differentiate ( independent, dependent )
        IMPLICIT NONE
        
        ! Input and output variables
        REAL(8), DIMENSION(:), INTENT(IN)   ::  dependent,                     &
                                                independent
        REAL(8), DIMENSION(:), ALLOCATABLE  ::  differentiate
        
        ! Local variables
        INTEGER ::  i,                                                         &
                    ret_size
    !--------------------------------------------------------------------------!
        
        ! Figure out how much data there is to process
        ret_size = MIN(SIZE(dependent),SIZE(independent))
        ALLOCATE(differentiate(1:ret_size))
        
        ! Calculate derivative for first data-point
        differentiate(1) = (dependent(2) - dependent(1))/(independent(2) -     &
            independent(1))
        
        ! Calculate derivative for data-points in the middle
        FORALL (i = 2:(ret_size - 1)) differentiate(i) = (dependent(i+1) -     &
            dependent(i-1))/(independent(i+1) - independent(i-1))
    
        ! Calculate the derivative for the last data-point
        differentiate(ret_size) = (dependent(ret_size) -                       &
            dependent(ret_size-1))/(independent(ret_size) -                    &
            independent(ret_size -1))
        
        RETURN
    END FUNCTION differentiate
    !==========================================================================!
    !                        E N D    F U N C T I O N :                        !
    !                        D I F F E R E N T I A T E                         !
    !==========================================================================!


    !==========================================================================!
    !                    B E G I N    S U B R O U T I N E :                    !
    !                                S T A T S                                 !
    !==========================================================================!
    !                                                                          !
    !   AUTHOR:         Christopher MacMackin                                  !
    !   WRITTEN:        July, 2014                                             !
    !   MODIFICATIONS:  None                                                   !
    !                                                                          !
    !   PURPOSE:        Finds the average and the standard deviation of the    !
    !                   data in the passed array.                              !
    !                                                                          !
    !   ARGUMENTS:     *array, a rank-1 REAL(8) array containing the data to   !
    !                       be averaged.                                       !
    !                   mean, the arithmetic mean of the data in the array.    !
    !                   stdev, the sample standard deviation of the data in    !
    !                       in the array.                                      !
    !                                                                          !
    !   EXTERNALS:      None                                                   !
    !                                                                          !
    !--------------------------------------------------------------------------!
    SUBROUTINE stats ( array, mean, stdev )
        IMPLICIT NONE
        
        ! Input and output variables
        REAL(8), DIMENSION(:), INTENT(IN)   ::  array
        REAL(8), INTENT(OUT)                ::  mean,                          &
                                                stdev
        
        ! Local variables
        INTEGER ::  i,                                                         &
                    num
        REAL(8) ::  running_tot
    !--------------------------------------------------------------------------!

        ! Compute the mean
        num  = SIZE(array)
        mean = SUM(array) / REAL(num,8)
        
        ! Compute the standard deviation
        DO i = 1, num
            running_tot = running_tot + ( array(i) - mean )**2
        END DO
        stdev = SQRT(1.d0/REAL((num - 1),8) * running_tot)
        
        RETURN
    END SUBROUTINE stats
    !==========================================================================!
    !                      E N D    S U B R O U T I N E :                      !
    !                                S T A T S                                 !
    !==========================================================================!

END MODULE tools
!==============================================================================!
!                            E N D    M O D U L E :                            !
!                                  T O O L S                                   !
!==============================================================================!
