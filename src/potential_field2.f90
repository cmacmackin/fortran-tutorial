!==============================================================================!
!                         B E G I N    P R O G R A M :                         !
!                        P O T E N T I A L _ F I E L D                         !
!==============================================================================!
!                                                                              !
!   AUTHOR:         Christopher MacMackin                                      !
!   WRITTEN:        October, 2014                                              !
!   MODIFICATIONS:  None                                                       !
!                                                                              !
!   PURPOSE:        Processes data on potential to calculate a field. Then     !
!                   finds some statistics on the field. The input file should  !
!                   consist of two columns of data, separated by spaces. The   !
!                   first column should be a position and the second should be !
!                   the potential at that position. All values should be in SI !
!                   units. The default input file is 'in.dat' and the default  !
!                   output file is 'out.dat'. Optionally, these may be         !
!                   overridden by providing these two file names as arguments  !
!                   when executing the program.                                !
!                                                                              !
!                   I.e.  $ ./potential_field [input_file [output_file]]       !
!                                                                              !
!   EXTERNALS:      tools (module)                                             !
!                                                                              !
!------------------------------------------------------------------------------!
PROGRAM potential_field
    USE tools
    IMPLICIT NONE

    ! Variable declarations
    CHARACTER(LEN=58), PARAMETER    ::  filedne = '("POTENTIAL_FIELD: Error, &
                                        &file ''",A,"'' does not exist.")'
    CHARACTER(LEN=218), PARAMETER   ::  output = '("POTENTIAL_FIELD: The &
                                        &average electric field strength was ",&
                                        &1pg12.5," N/C, with",/,&
                                        &"POTENTIAL_FIELD: a sample standard & 
                                        &deviation of ",1pg12.5," N/C.",/,& 
                                        &"POTENTIAL_FIELD: This was calculated &
                                        &using ",I4," data-points.")'
    CHARACTER(LEN=32)               ::  infile  = 'in.dat',                    &
                                        outfile = 'out.dat'
    INTEGER, PARAMETER              ::  data_max = 1012
    INTEGER                         ::  i,                                     &
                                        ioval,                                 &
                                        num_args,                              &
                                        data_size = data_max
    REAL(8)                         ::  mean,                                  &
                                        stdev
    REAL(8), DIMENSION(data_max)    ::  field,                                 &
                                        postn,                                 &
                                        potntl
!------------------------------------------------------------------------------!

    ! Get the input and output file-names from the command-line
    num_args = COMMAND_ARGUMENT_COUNT()
    IF ( num_args >= 1 ) CALL GET_COMMAND_ARGUMENT(1,infile)
    IF ( num_args >= 2 ) CALL GET_COMMAND_ARGUMENT(2,outfile)
    
    ! Read in data from the input file
    OPEN(UNIT=10, FILE=infile, IOSTAT=ioval, STATUS='old')
    IF ( ioval /= 0 ) THEN ! Make sure file exists
        WRITE(0, filedne) TRIM(infile)
        STOP
    END IF
    
    DO i = 1,data_max ! Read until end of file or reach maximum amount of data
        READ(10,*,IOSTAT=ioval) postn(i), potntl(i)
        IF ( ioval /= 0 ) THEN
            data_size = i - 1
            EXIT
        END IF
        IF ( i == data_max ) WRITE(6,*) 'POTENTIAL_FIELD: Could not read '//   &
            'all input data. Truncated after ', data_max, ' elements.'
    END DO
    CLOSE(10)
    
    ! Calculate the derivative of the input data
    field(1:data_size) = differentiate(postn(1:data_size), potntl(1:data_size))
    field(1:data_size) = -1.0 * field(1:data_size)

    ! Send the data to the output file
    OPEN(10, FILE=outfile, STATUS='unknown')
    WRITE(10,*) '#Position                   Field Strength'
    DO i = 1, data_size
        WRITE(10,*) postn(i), field(i)
    END DO
    CLOSE(10)
    
    ! Calculate the statistics and print results to screen
    CALL stats(field(1:data_size), mean, stdev)
    WRITE(6,output) mean, stdev, data_size
    
    STOP
END PROGRAM potential_field    
!==============================================================================!
!                           E N D    P R O G R A M :                           !
!                        P O T E N T I A L _ F I E L D                         !
!==============================================================================!
