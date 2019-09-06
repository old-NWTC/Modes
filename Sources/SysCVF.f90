MODULE SysSubs
   USE                             SysMod

   IMPLICIT                        NONE

!=======================================================================

CONTAINS

!=======================================================================
SUBROUTINE FindLine ( Str , MaxLen , StrEnd )

      ! This routine finds one line of text with a maximum length of MaxLen from the Str.
      ! It tries to break the line at a blank.

      ! This routine isn't system specific, but it is called by WrScr(), which is, so it must be here.


   IMPLICIT                        NONE


      ! Argument declarations:

   INTEGER(4), INTENT(IN)          :: MaxLen                                       ! The maximum length of the string.
   INTEGER(4), INTENT(OUT)         :: StrEnd                                       ! The location of the end of the string.

   CHARACTER(*), INTENT(IN)     :: Str                                          ! The string to search.


      ! Local declarations:

   INTEGER(4)         IC



   StrEnd = MaxLen

   IF ( LEN_TRIM( Str ) > MaxLen )  THEN

      IC = INDEX( Str(1:MaxLen), ' ', BACK = .TRUE. ) ! Find the last space in the line

      IF ( IC > 1 ) THEN ! We don't want to return just one character that's a space, or do we?

         StrEnd = IC-1    ! StrEnd > 0
         DO WHILE ( Str(StrEnd:StrEnd) == ' ' )
            StrEnd = StrEnd - 1
            IF ( StrEnd <= 0 ) THEN  ! This occurs if everything before IC is a space
               StrEnd = IC
               EXIT
            ENDIF
         ENDDO

      ENDIF ! IC > 1

   ENDIF ! LEN_TRIM( Str ) > MaxLen


   RETURN
   END SUBROUTINE FindLine ! ( Str , MaxLen , StrEnd )
!=======================================================================
SUBROUTINE FlushOut ( Unit )


!  This subroutine flushes the buffer on the specified Unit.
!  It is especially useful when printing "running..." type
!  messages.  By making this a separate routine, we isolate
!  ourselves from the OS and make porting easier.


IMPLICIT        NONE

INTEGER         Unit



CALL FLUSH ( Unit )


RETURN
END SUBROUTINE FlushOut
!=======================================================================
SUBROUTINE Get_Arg ( Arg_Num , Arg , Error )


!  This routine gets argument #Arg_Num from the command line.


IMPLICIT        NONE

INTEGER         Arg_Num
INTEGER         Status

LOGICAL         Error

CHARACTER(*)    Arg



CALL GET_COMMAND_ARGUMENT( Arg_Num , Arg , Status )

IF ( LEN_TRIM( Arg ) .GT. 0 .or. Status /=0 )  THEN
   Error = .FALSE.
ELSE
   Error = .TRUE.
ENDIF


RETURN
END SUBROUTINE Get_Arg
!=======================================================================
SUBROUTINE Get_Arg_Num ( Arg_Num )


!  This routine gets the number of command line arguments.



IMPLICIT        NONE

INTEGER         Arg_Num



Arg_Num = COMMAND_ARGUMENT_COUNT()


RETURN
END SUBROUTINE Get_Arg_Num
!=======================================================================
SUBROUTINE OpenCon


!  This routine opens the console for standard output.


IMPLICIT        NONE


!bjj: Because UC = 6 now, this statement is not necessary, and it can be system agnostic now.
!OPEN ( UC , FILE='CON' , STATUS='UNKNOWN' , CARRIAGECONTROL='FORTRAN' )

CALL FlushOut ( UC )


RETURN
END SUBROUTINE OpenCon
!=======================================================================
SUBROUTINE UsrAlarm


!  This routine generates an alarm to warn the user that
!  something went wrong.


IMPLICIT        NONE



CALL WrML ( CHAR( 7 ) )


RETURN
END SUBROUTINE UsrAlarm
!=======================================================================
SUBROUTINE WrML ( Str )


!  This routine writes out a string in the middle of a line.


CHARACTER(*)    Str



CALL WrNR ( Str )


RETURN
END SUBROUTINE WrML
!=======================================================================
SUBROUTINE WrNR ( Str )


!  This routine writes out a string to the screen without fol-
!  lowing it with a new line.



CHARACTER(*)    Str



WRITE (UC,'(1X,A)',ADVANCE='NO')  Str


RETURN
END SUBROUTINE WrNR
!=======================================================================
SUBROUTINE WrScr ( Str )



      ! This routine writes out a string to the screen.


   IMPLICIT                        NONE


      ! Argument declarations.

   CHARACTER(*), INTENT(IN)     :: Str                                          ! The string to write to the screen.


      ! Local declarations.

   INTEGER(4)                      :: Beg                                          ! The beginning of the next line of text.
   INTEGER(4)                      :: Indent                                       ! The amunt to be indented.
   INTEGER(4)                      :: LStr                                         ! The length of the remaining portion of the string.
   INTEGER(4)                      :: MaxLen                                       ! Maximum number of columns to be written to the screen.

   CHARACTER(10)                :: Frm                                          ! Format specifier for the output.



      ! Find the amount of indent.  Create format.

   MaxLen = 98
   Indent = LEN_TRIM( Str ) - LEN_TRIM( ADJUSTL( Str ) )
   MaxLen = MaxLen - Indent
   Frm    = '(1X,  X,A)'
   WRITE (Frm(5:6),'(I2)')  Indent



   !  Break long messages into multiple lines.

   Beg  = Indent + 1
   LStr = LEN_TRIM( Str(Beg:) )

   DO WHILE ( Lstr > MaxLen )

      CALL FindLine ( Str(Beg:) , MaxLen , LStr )

      WRITE (UC,Frm)  TRIM( ADJUSTL( Str(Beg:Beg+LStr-1) ) )

      Beg = Beg + LStr


         ! If we have a space at the beginning of the string, let's get rid of it

      DO WHILE ( Beg < LEN_TRIM( Str ) .AND. Str(Beg:Beg) == ' ' )
         Beg = Beg + 1
      ENDDO

      LStr = LEN_TRIM( Str(Beg:) )

   ENDDO

   if (Beg+LStr-1.ge.Beg) then
      WRITE (UC,Frm)  TRIM( ADJUSTL( Str(Beg:Beg+LStr-1) ) ) 
   endif   


   RETURN
END SUBROUTINE WrScr

END MODULE SysSubs
