# HDF5 wrapper
## HDF5 Installation
Download the tar ball from the [official homepage](https://support.hdfgroup.org/HDF5/).
Installation for e.g. gnu compiler is done via

```
tar xf hdf5-x.y.z.tar
cd hdf5-x.y.z
CC=gcc
FC=gfortran
./configure --enable-fortran -prefix=/opt/hdf5-x.y.z_gcc
make
make check
make install
```

## Wrapper installation
Compile the file `hdf5_wrapper.f90` with proper linking to your local `hdf5 `library.
In your program use the binding `use hdf5_wrapper` to use the wrapper (see test_example.f90).
A minimalistic compilation script can be found in `compile.sh`.

## HDF5 interface and file handling

| hdf5 command                 | description                        |
|------------------------------|------------------------------------|
| `hdf5_init()`                | initialize hdf5 interface          |
| `hdf5_finalize()`            | close hdf5 interface               |
| `hdf5_create_file(fname)`    | create hdf5 file                   |
| `hdf5_open_file(fname, ifile [,rdonly])`             | open hdf5 file                     |
| `hdf5_close_file(ifile)`             | close hdf5 file                     |

`fname` represents a Fortran string (character array) while `ifile` is a an integer of the form
`integer(hid_t)` (hence we also need to make the hdf5 library available outside the wrapper).

```
program filehandling
	use hdf5
	use hdf5_wrapper

	integer(hid_t) :: ifile

	call hdf5_init()
	call hdf5_create_file('test.hdf5')
	call hdf5_open_file('test.hdf5', ifile)
	call hdf5_close_file(ifile)
	call hdf5_finalize()
end program
```

## Groups

| hdf5 command                 | description                        |
|------------------------------|------------------------------------|
| `hdf5_create_group(ifile, gname)`          | create group(s) (parents included) |
| `hdf5_list_groups(ifile, gname)`           | get list of groups                 |
| `hdf5_get_number_groups(ifile, gname)`     | get number of lists                |

We are able to create groups by simply providing the file identifier `ifile` and the full (unix-like) path.
The wrapper checks for the existence of parent groups and creates them if they do not exist.

```
program groups
	use hdf5
	use hdf5_wrapper

	integer(hid_t) :: ifile
	integer :: ngroups
	character(len=100), allocatable :: list(:)

	call hdf5_init()
	call hdf5_create_file('test.hdf5')
	call hdf5_open_file('test.hdf5', ifile)

	call hdf5_create_group(ifile, '/group1/group2/group3')
	call hdf5_create_group(ifile, '/group1/group4')
	call hdf5_create_group(ifile, '/group1/group5')

 	! the list array gets allocated and afterwards contains the group names
	call hdf5_list_groups(ifile, '/group1', list)
	ngroups = hdf5_get_number_groups(ifile, '/group1') ! 3

	call hdf5_close_file(ifile)
	call hdf5_finalize()
end program
```

The resulting file can be inspected by `h5ls -lr`.

## Datasets

| hdf5 command                 | description                        |
|------------------------------|------------------------------------|
| `hdf5_read_data(ifile, dname, adata)`             | read data from dataset             |
| `hdf5_write_data(ifile, dname, data)`            | write datasets from arrays             |
| `hdf5_list_datasets(ifile, group, list)`         | get list of datasets             |
| `hdf5_get_number_datasets(ifile, group)`   | get number of datasets             |
| `hdf5_get_dimensions(ifile, dataset)`        | get number of dimensions           |
| `hdf5_get_shape(ifile, dataset, shape_array)`             | get shape in form of an array      |

The dataset functions work in the same way as the group functions. One can create datasets with a full
(unix-like) path where, again, the non-existent parent groups are created on the fly.
We provide wrappers for the following datatypes for datasets:

* `integer(4)`
* `real(4)`
* `real(8)`
* `complex(4)`
* `complex(8)`

The in/output can be done for `0D` to `7D` (Fortran maximum) arrays. The readin functions
work the same way, only that we have to provide an allocatable array.
Please note that there is no check for matching hdf5 and fortran datatypes.

```
program datasets
	use hdf5
	use hdf5_wrapper

	complex(8) :: a(3,5,2) = (1.2d0, 0.2d0)
	complex(8), allocatable :: b(:,:,:)
	integer(hid_t) :: ifile
	integer :: ngroups, dimensions
	character(len=100), allocatable :: list(:)
	integer, allocatable :: hdf_shape(:)

	call hdf5_init()
	call hdf5_create_file('test.hdf5')
	call hdf5_open_file('test.hdf5', ifile)

	call hdf5_write_data(ifile, '/group1/group2/dataset', a)
	call hdf5_read_data(ifile, '/group1/group2/dataset', b)

	! same as for the groups
	call hdf5_list_datasets(ifile, '/group1/group2', list)
	ngroups = hdf5_get_number_datasets(ifile, '/group1/group2') ! 1

	! dimensions and shape
	call hdf5_get_shape(ifile, '/group1/group2/dataset', hdf_shape)
	dimensions = hdf5_get_dimensions

	call hdf5_close_file(ifile)
	call hdf5_finalize()
end program
```


## Attributes

| hdf5 command                 | description                        |
|------------------------------|------------------------------------|
| `hdf5_write_attribute(ifile, location, attr_name, attribute)`       | write attribute                    |
| `hdf5_read_attribute(ifile, location, attr_name, variable)`        | read attribute                     |
| `hdf5_get_number_attributes(ifile, location)`        | get number of attributes                     |

We also provide a small interface for reading and writing attributes.
The following datatypes are supported:

* `integer(4)`
* `real(4)`
* `real(8)`
* `complex(4)`
* `complex(8)`
* `character(len=*)`

Attributes can be attached to both groups and datasets. For this reason
we do not create anything on the fly. Any non-existance of objects will cause an error.

```
program attributes
	use hdf5
	use hdf5_wrapper

	integer(4) :: x = 3
	real(8)    :: y

	call hdf5_init()
	call hdf5_create_file('test.hdf5')
	call hdf5_open_file('test.hdf5', ifile)

	call hdf5_write_data(ifile, '/group1/dataset', x)

	! attach to group
	call hdf5_write_attribute(ifile, '/group1', 'attr_name1', 1.23d0)
	! attach to dataset
	call hdf5_write_attribute(ifile, '/group1/dataset', 'attr_name2', 'astring')

	! read the above attribute into the variable y
	call hdf5_read_attribute(ifile, '/group1', 'attr_name1', y)

	call hdf5_close_file(ifile)
	call hdf5_finalize()
end program
```

## Summary of commands

| hdf5 command                 | description                        |
|------------------------------|------------------------------------|
| `hdf5_init()`                | initialize hdf5 interface          |
| `hdf5_finalize()`            | close hdf5 interface               |
| `hdf5_create_file(fname)`    | create hdf5 file                   |
| `hdf5_open_file(fname, ifile [,rdonly])`             | open hdf5 file                     |
| `hdf5_close_file(ifile)`             | close hdf5 file                     |
| `hdf5_create_group(ifile, gname)`          | create group(s) (parents included) |
| `hdf5_list_groups (ifile, gname)`           | get list of groups                 |
| `hdf5_get_number_groups (ifile, gname)`     | get number of lists                |
| `hdf5_read_data(ifile, dname, adata)`             | read data from dataset             |
| `hdf5_write_data(ifile, dname, data)`            | write datasets from arrays             |
| `hdf5_list_datasets(ifile, group, list)`         | get list of datasets             |
| `hdf5_get_number_datasets(ifile, group)`   | get number of datasets             |
| `hdf5_get_dimensions(ifile, dataset)`        | get number of dimensions           |
| `hdf5_get_shape(ifile, dataset, shape_array)`             | get shape in form of an array      |
| `hdf5_write_attribute(ifile, location, attr_name, attribute)`       | write attribute                    |
| `hdf5_read_attribute(ifile, location, attr_name, variable)`        | read attribute                     |
| `hdf5_get_number_attributes(ifile, location)`        | get number of attributes                     |

## Future Features

* `hdf5_list_attributes`
* `hdf5_write_data(ifile, dname, data [,gzip])` with **manual** chunking.
* Loading of partial data
