program bla
  use hdf5_wrapper
  use hdf5
  implicit none

  integer(hid_t) :: ifile

  complex(8)                      :: a(3,3,1,1)
  complex(8), allocatable         :: a_read(:,:,:,:)

  integer                         :: b = 5
  real(8)                         :: c = 123.d0
  integer                         :: d
  integer                         :: i

  logical, allocatable            :: arr_logical(:,:)
  character(len=100), allocatable :: list(:)


  a = (1.23d0, 0.11d0)
  a(:,1,1,1) = 99.d0

  allocate(arr_logical(3,3))
  arr_logical = .false.
  arr_logical(3,:) = .true.


  ! initialize hdf5 interface
  write(*,*) 'Initializing hdf5 interface'
  call hdf5_init()
  ! create file
  write(*,*) 'Creating file test.hdf5'
  call hdf5_create_file('test.hdf5')
  ! open file with file identifier ifile
  write(*,*) 'Opening file test.hdf5'
  call hdf5_open_file('test.hdf5', ifile)
  write(*,*) 'Identifier: ', ifile
  write(*,*)

  ! create groups (parent groups get created automatically)
  write(*,*) 'Creating nested groups'
  call hdf5_create_group(ifile, 'group1/group2')

  ! writing datasets into root
  write(*,*) 'Writing integer dataset'
  write(*,*) b
  call hdf5_write_data(ifile, 'dataset_integer', b)
  write(*,*) 'Reading integer dataset'
  call hdf5_read_data(ifile, 'dataset_integer', i) ! read into logical
  write(*,*) i

  ! writing arrays (parent groups get created automatically)
  ! we are able to write int4, real4, real8, complex4, complex8
  write(*,*) 'Writing double precision complex dataset with automatic parent group creation'
  call hdf5_write_data(ifile, 'group3/dataset_doublecomplex', a)

  write(*,*) 'Writing double precision real dataset'
  call hdf5_write_data(ifile, 'dataset_double', c)

  write(*,*) 'Writing logical dataset'
  write(*,*) arr_logical
  call hdf5_write_data(ifile, 'dataset_logical', arr_logical)
  deallocate(arr_logical)
  write(*,*) 'Reading logical dataset'
  call hdf5_read_data(ifile,  'dataset_logical', arr_logical)
  write(*,*) arr_logical



  ! writing attributes
  write(*,*) 'Writing attributes'
  call hdf5_write_attribute(ifile, 'group1', 'att1', 'string')
  call hdf5_write_attribute(ifile, 'group1', 'att2', (1.0, 2.0))
  call hdf5_list_attributes(ifile, 'group1', list)

  call hdf5_write_attribute(ifile, 'group1/group2', 'att2', 123)
  call hdf5_write_attribute(ifile, 'dataset_logical', 'att3', 1.22d-2)
  call hdf5_write_attribute(ifile, 'dataset_logical', 'att4', .true.)
  call hdf5_write_attribute(ifile, 'dataset_logical', 'att5', .false.)

  write(*,*) 'List of written attributes into /group1'
  do i = 1, size(list)
    write(*,*) trim(list(i))
  enddo
  deallocate(list)


  write(*,*) 'Checking existence of written gropus / datasets / attributes'
  write(*,*) 'required: actual value'
  write(*,*) 'T: ', hdf5_group_exists(ifile, 'group1')
  write(*,*) 'T: ', hdf5_group_exists(ifile, 'group1/group2')
  write(*,*) 'F: ', hdf5_group_exists(ifile, 'group_which_does_not_exist')
  write(*,*) 'F: ', hdf5_dataset_exists(ifile, 'group1')
  write(*,*) 'T: ', hdf5_dataset_exists(ifile, 'dataset_logical')
  write(*,*) 'F: ', hdf5_dataset_exists(ifile, 'dataset_which_does_not_exit')
  write(*,*) 'F: ', hdf5_group_exists(ifile, 'dataset_logical')
  write(*,*) 'T: ', hdf5_attribute_exists(ifile, 'dataset_logical', 'att3')
  write(*,*) 'F: ', hdf5_attribute_exists(ifile, 'dataset_logical', 'att_that_does_not_exit')


  write(*,*) 'Deleting attribute'
  write(*,*) 'before - T: ', hdf5_attribute_exists(ifile, 'dataset_logical', 'att3')
  call hdf5_delete_attribute(ifile, 'dataset_logical', 'att3')
  write(*,*) 'after  - F: ', hdf5_attribute_exists(ifile, 'dataset_logical', 'att3')

  write(*,*) 'Closing file'
  call hdf5_close_file(ifile)




  ! open the file again in readonly mode
  write(*,*)
  write(*,*)
  write(*,*) 'Opening file in read-only'
  call hdf5_open_file('test.hdf5', ifile, rdonly=.true.)

  ! we provide an allocatable array of characters
  write(*,*) 'groups in / ---'
  call hdf5_list_groups(ifile, '/', list)
  do i = 1, size(list)
    write(*,*) trim(list(i))
  enddo
  deallocate(list)
  write(*,*) '--- groups'

  write(*,*)
  write(*,*) 'dataset in / ---'
  call hdf5_list_datasets(ifile, '/', list)
  do i = 1, size(list)
    write(*,*) trim(list(i))
  enddo
  deallocate(list)
  write(*,*) '--- datasets'


  ! loading normal arrays works the same way, we need an allocatable array
  write(*,*) 'Reading double complex dataset'
  call hdf5_read_data(ifile, '/group3/dataset_doublecomplex', a_read)
  write(*,*) 'Allocated: ', allocated(a_read)
  if (allocated(a_read)) then
    write(*,*) 'Data: '
    write(*,*) a_read
  endif

  ! closing again
  write(*,*) 'Closing file in read-only'
  call hdf5_close_file(ifile)
  ! closing the hdf5 interface
  write(*,*) 'Closing hdf5 interface'
  call hdf5_finalize()

end program
