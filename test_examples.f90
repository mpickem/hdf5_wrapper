program bla
  use hdf5_wrapper
  use hdf5
  implicit none

  integer(hid_t) :: ifile

  complex(8) :: a(3,3,3,3,1,1)
  integer    :: b = 5
  real(8)    :: c = 123.d0

  complex(8), allocatable :: a_read(:,:,:,:,:,:)
  character(len=100), allocatable :: list(:)

  integer :: i

  a = (1.23d0, 0.11d0)


  ! initialize hdf5 interface
  call hdf5_init()
  ! create file
  call hdf5_create_file('test.hdf5')
  ! open file with file identifier ifile
  call hdf5_open_file('test.hdf5', ifile)


  ! create groups (parent groups get created automatically)
  call hdf5_create_group(ifile, 'group1/group2')
  ! writing arrays (parent groups get created automatically)
  ! we are able to write int4, real4, real8, complex4, complex8
  call hdf5_write_data(ifile, 'group3/array', a)
  call hdf5_write_data(ifile, 'dataset1', b)
  call hdf5_write_data(ifile, 'dataset2', c)

  ! writing attributes
  call hdf5_write_attribute(ifile, 'group1', 'att1', 'string')
  call hdf5_write_attribute(ifile, 'group1', 'att2', (1.0, 2.0))

  call hdf5_list_attributes(ifile, 'group1', list)

  if (allocated(list)) then
    write(*,*) list
    deallocate(list)
  endif

  call hdf5_write_attribute(ifile, 'group1/group2', 'att2', 123)
  call hdf5_write_attribute(ifile, 'dataset1', 'att3', 1.22d-2)

  ! close the file again
  call hdf5_close_file(ifile)



  ! open the file again in readonly mode
  call hdf5_open_file('test.hdf5', ifile, rdonly=.true.)

  ! we provide an allocatable array of characters
  call hdf5_list_groups(ifile, '/', list)
  do i = 1, size(list)
    write(*,*) trim(list(i))
  enddo
  deallocate(list)


  ! loading normal arrays works the same way, we need an allocatable array
  call hdf5_read_data(ifile, '/group3/array', a_read)
  write(*,*) allocated(a_read)
  if (allocated(a_read)) write(*,*) a_read

  ! closing again
  call hdf5_close_file(ifile)
  ! closing the hdf5 interface
  call hdf5_finalize()

end program
