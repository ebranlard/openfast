module NWTC_IO_Bis
   USE NWTC_IO
   !CHARACTER(*), PARAMETER       :: CommChars = '!#%'                            !< Comment characters that mark the end of useful input
   implicit none

   type, public :: TabInfo
      character(64)  :: name = ''
      character(64)  :: format =''
      integer(IntKi) :: col = 0
      integer(IntKi) :: row = 0
      integer(IntKi) :: header = 0
   end type TabInfo

   integer, parameter :: TableStrLen = 255 !< Maximum String length in tables
   integer, parameter :: MaxLineLen = 1024 !< Maximum line length on a file

contains

   !> Read a tbale header line, starting with `#table` and with key:values
   !! The keys are: name, header, col, row, format 
   !! NOTE: limited error handling is done to allow for "missing fields", and to allow the
   !! programmer to override the "Info" structure after this call.
   subroutine ReadTableHeader ( UnIn, Fil, Info, ErrStat, ErrMsg, UnEc)
      integer,        intent(in)            :: UnIn    !< I/O unit for input file.
      integer,        intent(in), optional  :: UnEc    !< I/O unit for echo file. If present and > 0, write to UnEc
      character(*),   intent(in)            :: Fil     !< Name of the input file.
      type(TabInfo),  intent(inout)         :: Info    !< Tab info
      integer(intki), intent(out)           :: ErrStat !< Error status; if present, program does not abort on error
      character(*),   intent(out)           :: ErrMsg  !< Error message
      ! Local declarations:
      integer :: IOS, i, n
      character(1024) :: Line      !< Text string containing the comment.
      character(64), dimension(5) :: SubStrings

      ! Read full line
      read (UnIn,'(A)',iostat=IOS)  Line
      call CheckIOS ( IOS, Fil, 'TableLine', StrType, ErrStat, ErrMsg )
      if (ErrStat >= AbortErrLev) return

      call Conv2UC(Line)
      
      if ( len(Line) < 7 ) then
         call Fatal('Table needs to start with `#table` '); return
      endif
      if ( Line(1:7) /= '#TABLE' ) then
         call Fatal('Table needs to start with `#table` '); return
      endif

      ! Extract substrings
      call ReadCAryFromStr(Line(7:len(Line)), SubStrings, 5, 'SubStrings', '', ErrStat, ErrMsg)
      !if (ErrStat >= AbortErrLev) return ! NOTE: NO ERROR HANDLING, we expect at max 5 fields

      ! Insert fields into TabInfo
      do i = 1, size(SubStrings)
         n=len(SubStrings(i))
         if (n==0) continue

         if     (index(SubStrings(i), 'NAME')>0) then
            Info%name=SubStrings(i)(6:n)

         elseif (index(SubStrings(i), 'FORMAT')>0) then
            Info%format=SubStrings(i)(8:n)

         elseif (index(SubStrings(i), 'ROW')>0) then
            read( SubStrings(i)(5:n), *, iostat=IOS) Info%row
            if (IOS/=0) then
               call Fatal('Invalid input for key `row` '); return
            endif

         elseif (index(SubStrings(i), 'COL')>0) then
            read( SubStrings(i)(5:n), *, iostat=IOS) Info%col
            if (IOS/=0) then
               call Fatal('Invalid input for key `col` '); return
            endif

         elseif (index(SubStrings(i), 'HEADER')>0) then
            read( SubStrings(i)(8:n), *, iostat=IOS) Info%header
            if (IOS/=0) then
               call Fatal('Invalid input for key `header` '); return
            endif
         !else
         !   call Fatal('Keyword unrecognized `'//trim(SubStrings(i))//'`'); return
         endif
      enddo

      if ( present(UnEc) )  then
         if ( UnEc > 0 ) &
            write (UnEc,'("#table name:",A," format:",A, " col:",I0, " row:",I0, " header:",I0)') trim(Info%name), trim(Info%format), Info%col, Info%row, Info%header
      end if
   contains

      subroutine Fatal(ErrMsg_in)
         character(len=*), intent(in) :: ErrMsg_in
         ErrStat=ErrID_Fatal
         ErrMsg ='Error reading table header: '//trim(ErrMsg_in)//', in file "'//trim(Fil)//'", problematic line: `'//trim(Line)//'`'
      end subroutine Fatal

   end subroutine ReadTableHeader
   

   !> Read a tbale header line, starting with `#table` and with key:values
   !! The keys are: name, header, col, row, format 
   subroutine ReadTable(UnIn, Fil, Info, ErrStat, ErrMsg, UnEc, IArray, FArray, BArray, SArray)
      integer,        intent(in)            :: UnIn    !< I/O unit for input file.
      integer,        intent(in), optional  :: UnEc    !< I/O unit for echo file. If present and > 0, write to UnEc
      character(*),   intent(in)            :: Fil     !< Name of the input file.
      type(TabInfo),  intent(in)            :: Info    !< Tab info
      integer(intki), intent(out)           :: ErrStat !< Error status; if present, program does not abort on error
      character(*),   intent(out)            :: ErrMsg  !< Error message
      integer(IntKi), dimension(:,:), allocatable, optional :: IArray !< Array of integers
      real(ReKi),     dimension(:,:), allocatable, optional :: FArray !< Array of floats
      logical,        dimension(:,:), allocatable, optional :: BArray !< Array of logical
      character(TableStrLen), dimension(:,:), allocatable, optional :: SArray !< Array of strings
      ! Local declarations:
      integer :: IOS, n
      character(MaxLineLen) :: Line  !< Text string containing the comment.
      character(TableStrLen), allocatable :: StrArray(:) ! Array of strings extracted from line
      real(ReKi)                 :: DummyFloat
      integer(IntKi)             :: J, nColI, nColF, nColB, nColS, I
      integer(IntKi)             :: ErrStat2
      character(ErrMsgLen)       :: ErrMsg2
      character(255)             :: InfoLine
      nColI = count((/ (Info%format(i:i), i=1,len(Info%format)) /)=='I')
      nColF = count((/ (Info%format(i:i), i=1,len(Info%format)) /)=='F')
      nColB = count((/ (Info%format(i:i), i=1,len(Info%format)) /)=='B')
      nColS = count((/ (Info%format(i:i), i=1,len(Info%format)) /)=='S')

      ! --- Check that formats are supported based on provided arrays
      if ((nColI>0) .and. .not.present(IArray)) then
         call Fatal('Integer format not allowed for this table')
      endif
      if ((nColF>0) .and. .not.present(FArray)) then
         call Fatal('Float format not allowed for this table')
      endif
      if ((nColB>0) .and. .not.present(BArray)) then
         call Fatal('Boolean format not allowed for this table')
      endif
      if ((nColS>0) .and. .not.present(SArray)) then
         call Fatal('String format not allowed for this table')
      endif

      ! Checks that Table info is consistent
      if (len_trim(Info%format)/=Info%col) then
            call Fatal('Length of `format` string ('//trim(Info%format)//') should match `col` value ('//trim(num2lstr(Info%col))//') for table '//trim(Info%name))
      endif

      ! --- Allocating tables
      if (present(IArray)) then
         call AllocAry(IArray, Info%row, nColI, 'IArray', ErrStat2, ErrMsg2); if(Failed()) return
      endif
      if (present(FArray)) then
         call AllocAry(FArray, Info%row, nColF, 'FArray', ErrStat2, ErrMsg2); if(Failed()) return
      endif
      if (present(BArray)) then
         call AllocAry(BArray, Info%row, nColB, 'BArray', ErrStat2, ErrMsg2); if(Failed()) return
      endif
      if (present(SArray)) then
         call AllocAry(SArray, Info%row, nColS, 'SArray', ErrStat2, ErrMsg2); if(Failed()) return
      endif
      CALL AllocAry(StrArray, Info%col, 'StrArray', ErrStat2, ErrMsg2); if(Failed()) return


      ! --- Loop on file lines
      ! Omit headers
      do I=1, Info%header
         ! TODO Use a new readline
         read (UnIn,'(A)',iostat=IOS)  Line
         call CheckIOS ( IOS, Fil, InfoLine, StrType, ErrStat2, ErrMsg2); if(Failed()) return
         if ( present(UnEc) )  then
            if ( UnEc > 0 ) write(UnEc, '(A)') trim(Line)
         end if
      enddo

      ! --- Loop on file lines
      do I=1, Info%row
         InfoLine = 'Table:'//trim(Info%name)//' Line:'//trim(num2lstr(I))
         ! Read full line as string
         read (UnIn,'(A)',iostat=IOS)  Line
         call CheckIOS ( IOS, Fil, InfoLine, StrType, ErrStat2, ErrMsg2); if(Failed()) return
         if ( present(UnEc) )  then
            if ( UnEc > 0 ) write(UnEc, '(A)') trim(Line)
         end if

         ! Extract substrings
         StrArray(:)='';
         CALL ReadCAryFromStr(Line, StrArray, Info%col, 'StrArray', InfoLine, ErrStat2, ErrMsg2); if(Failed()) return

         nColI=0; nColF=0; nColB=0; nColS=0; ! indices
         ! Perform type conversion
         do J = 1, Info%col
            InfoLine = trim(InfoLine)//' Column:'//trim(num2lstr(J))
            if     (Info%format(J:J)=='I') then
               nColI = nColI+1
               read( StrArray(J), *, iostat=IOS) IArray(I, nColI)
               if (IOS/=0) then
                  call Fatal('Invalid integer value in '//trim(InfoLine), Line); return
               endif
            elseif (Info%format(J:J)=='F') then
               nColF = nColF+1
               read( StrArray(J), *, iostat=IOS) FArray(I, nColF)
               if (IOS/=0) then
                  call Fatal('Invalid float value in '//trim(InfoLine), Line); return
               endif
            elseif (Info%format(J:J)=='B') then
               nColB = nColB+1
               read( StrArray(J), *, iostat=IOS) BArray(I, nColB)
               if (IOS/=0) then
                  call Fatal('Invalid logical value in '//trim(InfoLine), Line); return
               endif
            elseif (Info%format(J:J)=='S') then
               nColS = nColS+1
               SArray(I, nColS) = StrArray(J)
            else
               call Fatal('Invalid format `'//Info%format(J:J)//'` for table columns'); return
            endif
         enddo ! loop on columns
      enddo ! reading rows
      if(allocated(StrArray)) deallocate(StrArray)

  contains
      logical function Failed()
           call SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, 'SD_JacobianPContState') 
           Failed =  ErrStat >= AbortErrLev
      end function Failed

      subroutine Fatal(ErrMsg_in, Line)
         character(len=*), intent(in) :: ErrMsg_in
         character(len=*), optional, intent(in) :: Line
         ErrStat=ErrID_Fatal
         ErrMsg ='Error reading table: '//trim(ErrMsg_in)//', in file "'//trim(Fil)
         if (present(Line)) then
            ErrMsg=trim(ErrMsg)//'". Problematic line: `'//trim(Line)//'`'
         endif
      end subroutine Fatal

   end subroutine ReadTable


   function is_numeric(string, x)
      character(len=*), intent(in) :: string
      real(reki), intent(out) :: x
      logical :: is_numeric
      integer :: e,n
      character(len=12) :: fmt
      x = 0.0_ReKi
      n=len_trim(string)
      write(fmt,'("(F",I0,".0)")') n
      read(string,fmt,iostat=e) x
      is_numeric = e == 0
   end function is_numeric

   function is_logical(string, b)
      character(len=*), intent(in) :: string
      logical, intent(out) :: b
      logical :: is_logical
      integer :: e,n
      b = .false.
      n=len_trim(string)
      read(string,*,iostat=e) b
      is_logical = e == 0
   end function is_logical


   FUNCTION IsCommentNew(StringToCheck)
         ! Note: only the first character in the word is checked. Otherwise we would falsely grab the units '(%)'
      LOGICAL                       :: IsCommentNew
      CHARACTER(*),   INTENT(IN  )  :: StringToCheck                          ! String to check
            
      if ( LEN_TRIM(StringToCheck) > 0 ) then
         IsCommentNew = INDEX( CommChars, StringToCheck(1:1) ) > 0

         if (.not.(IsCommentNew).and. (LEN_TRIM(StringToCheck) >= 3) ) then
            IsCommentNew = StringToCheck(1:3) == '---'
         endif
      else
         IsCommentNew = .FALSE.
      end if
      
   END FUNCTION IsCommentNew


   !> This routine reads a comment from the next line of the input file.
   SUBROUTINE ReadComNew ( UnIn, Fil, ComName, ErrStat, ErrMsg, UnEc, Comment )
      INTEGER,        INTENT(IN)            :: UnIn    !< I/O unit for input file.
      INTEGER,        INTENT(IN), OPTIONAL  :: UnEc    !< I/O unit for echo file. If present and > 0, write to UnEc
      CHARACTER(*),   INTENT(IN)            :: Fil     !< Name of the input file.
      CHARACTER(*),   INTENT(IN)            :: ComName !< Text string containing the comment name.
      INTEGER(IntKi), INTENT(OUT)           :: ErrStat !< Error status; if present, program does not abort on error
      CHARACTER(*),   INTENT(OUT)           :: ErrMsg  !< Error message
      CHARACTER(*),   INTENT(OUT), OPTIONAL :: Comment !< Text string containing the comment.
      ! Local declarations:
      INTEGER :: IOS                                             ! I/O status returned from the read statement.
      CHARACTER(1024) :: Line !< Text string containing the comment.


      if(present(Comment)) Comment=''
      READ (UnIn,'(A)',IOSTAT=IOS)  Line
      CALL CheckIOS ( IOS, Fil, ComName, StrType, ErrStat, ErrMsg )
      IF (ErrStat >= AbortErrLev) RETURN
      IF (.not. IsCommentNew(Line)) then
         ErrStat = ErrID_Fatal
         ErrMsg = 'Comment does not start with a comment character, while trying to read '//trim(ComName)//', in file "'//trim(Fil)//'", problematic line: '//trim(Line)
         RETURN
      endif
      if(present(Comment)) Comment=trim(Line)
      IF ( PRESENT(UnEc) )  THEN
         IF ( UnEc > 0 ) &
            WRITE (UnEc,'(A)')  trim(Line)
      END IF
      RETURN
   END SUBROUTINE ReadComNew

end module NWTC_IO_Bis

program test
   USE NWTC_IO
   USE NWTC_IO_Bis, only: ReadComNew, ReadTableHeader, ReadTable
   USE NWTC_IO_Bis, only: TabInfo, TableStrLen
   implicit none
   integer(IntKi)       :: UnIn, UnEc
   integer(IntKi)       :: ErrStat2, IOS
   logical              :: Bool
   character(ErrMsgLen) :: ErrMsg2
   character(ErrMsgLen) :: InputFile = './Test.dat'
   character(ErrMsgLen)    :: Line 
   type(TabInfo)         :: Info
   integer(IntKi), dimension(:,:), allocatable :: IArray !< Array of integers
   real(ReKi),     dimension(:,:), allocatable :: FArray !< Array of floats
   logical,        dimension(:,:), allocatable :: BArray !< Array of logical
   character(TableStrLen), dimension(:,:), allocatable :: SArray !< Array of strings
   ! Initialize ErrStat

   call GetNewUnit( UnIn )   
   call OpenFInpfile(UnIn, TRIM(InputFile), ErrStat2, ErrMsg2); call Check();

   call GetNewUnit( UnEc )   
   CALL OpenEcho ( UnEc, TRIM(InputFile)//'.ech' ,ErrStat2, ErrMsg2); call Check();

    call ReadComNew(UnIn, InputFile, 'Comment1', ErrStat2, ErrMsg2, UnEc); call Check();
    call ReadComNew(UnIn, InputFile, 'Comment2', ErrStat2, ErrMsg2, UnEc); call Check();
    call ReadComNew(UnIn, InputFile, 'Comment3', ErrStat2, ErrMsg2, UnEc); call Check();
    call ReadComNew(UnIn, InputFile, 'Comment4', ErrStat2, ErrMsg2, UnEc); call Check();
    call ReadTableHeader(UnIn, InputFile, Info, ErrStat2, ErrMsg2, UnEc); call Check()
    call ReadTable(UnIn, InputFile, Info, ErrStat2, ErrMsg2, UnEc, SArray=SArray, IArray=IArray, FArray=FArray); call Check()

   close(UnIn)
   close(UnEc)

contains
   subroutine Check()
      if (ErrStat2/=ErrID_None) then
         print*,ErrStat2,trim(ErrMsg2)
      endif
   end subroutine Check

end program
