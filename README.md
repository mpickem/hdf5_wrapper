# HDF5 wrapper
## HDF5 Installation
Download the tar ball from the [official homepage](http://portal.hdfgroup.org/display/support).
Make sure to always use the latest version (currently: 1.13.0). Older versions (1.8 and below) may result in unintentional problems, especially with non-native datatypes like `logical`.
Installation for e.g. gnu compiler is done via

```
tar xf hdf5-x.y.z.tar
cd hdf5-x.y.z
CC=gcc
FC=gfortran
./configure --enable-fortran --enable-fortran2003 --prefix=/opt/hdf5-x.y.z_gcc
make
make check
make install
```

For more detailed library information please refer to [[Libraries and tools reference]](http://portal.hdfgroup.org/display/HDF5/Libraries+and+Tools+Reference).

## Wrapper installation
Compile the file `hdf5_wrapper.F90` with proper linking to your local `hdf5` library.
In your program use the binding `use hdf5_wrapper` to use the wrapper (see `test_example.f90`).
A minimalistic compilation script can be found in `compile_gfortran.sh` and `compile_ifort.sh`.

## General thoughts
This wrapper tries to mimic unix-like pathing wherever possible.
Group and dataset access is done with the typical unix-like path formalism
(e.g. `/group1/group2` where the leading and trailing `/` are optional) similarly to the Python library `h5py`.
The data read-in routines can be used by simply providing allocatable data arrays of the most commonly used datatypes in Fortran (see below).
Please note that I did not include any dimension or datatype checks. While the majority of routines are subroutines (`call` keyword required)
some are simple functions which either return `logical` or `integer` datatypes.
These are marked accordingly in [Summary of commands](#Summary-of-commands).
### Implicit data transposition
Here I simply want to mention one of the biggest pitfalls when handling hdf5 files with Fortran, namely
the implicit data transposition. Already when creating a simple file with a multidimensional dataset
and looking at said file with an external tool (e.g. `h5ls -lr <filename>`) one notices that the
dataset dimensions are reversed. That means if we, inside Fortran, create a three-dimensional dataset with a shape of `[ 4, 8, 2 ]`,
inspecting it with `h5ls` will show a shape of `[ 2, 8, 4]`.

This is not a bug and simply illustrates
the difference between the row-major order of C, Python, etc. and the column-major order of Fortran.
When saving the dataset with Fortran it will be stored as the contiguous memory as Fortran sees it (column-major).
Opening the same dataset with other tools this data will be interpreted as row-major which causes the full transposition
of the dataset. This implicit data transposition is important to keep in mind when handling Fortran in combination
with other row-major languages
### A few handy commands
As already mentioned, when inspecting hdf5 files from the shell `h5ls` (installed with the hdf5 installation) is a good tip.
Here are some of my most commonly used commands:
* `h5ls -lr`: (**r**)ecursively (**l**)ist all groups and datasets
* `h5ls -vlr`: (**v**)erbosely and (**r**)ecursively (**l**)ist everything (this includes attributes, datatypes, etc.)
* `h5ls -d`: inspect the (**d**)ata directly by simply appending the full unix-like path after the file without space (e.g. `file.hdf5/group1/dset`)

## HDF5 interface and file handling

| hdf5 command                             | description                          |
| ---------------------------------------- | ------------------------------------ |
| `hdf5_init()`                            | initialize hdf5 interface            |
| `hdf5_finalize()`                        | close hdf5 interface                 |
| `hdf5_create_file(fname)`                | create hdf5 file                     |
| `hdf5_open_file(fname, ifile [,rdonly])` | open hdf5 file                       |
| `hdf5_close_file(ifile)`                 | close hdf5 file                      |

`fname` represents a Fortran string (character array) while `ifile` is a an integer of the
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

| hdf5 command                           | description                          |
| -------------------------------------- | ------------------------------------ |
| `hdf5_create_group(ifile, gname)`      | create group(s) (parents included)   |
| `hdf5_list_groups(ifile, gname)`       | get list of groups                   |
| `hdf5_get_number_groups(ifile, gname)` | get number of lists                  |
| `hdf5_group_exists(ifile, gname)`      | check if specific group exists       |

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

	write(*,*) hdf5_group_exists(ifile, 'group1/group2')

 	! the list array gets allocated and afterwards contains the group names
	call hdf5_list_groups(ifile, '/group1', list)
	ngroups = hdf5_get_number_groups(ifile, '/group1') ! 3

	call hdf5_close_file(ifile)
	call hdf5_finalize()
end program
```

The resulting file can be inspected by `h5ls -lr`.

## Datasets

| hdf5 command                                  | description                          |
| --------------------------------------------- | ------------------------------------ |
| `hdf5_read_data(ifile, dname, adata)`         | read data from dataset               |
| `hdf5_write_data(ifile, dname, data)`         | write datasets from arrays           |
| `hdf5_list_datasets(ifile, group, list)`      | get list of datasets                 |
| `hdf5_get_number_datasets(ifile, group)`      | get number of datasets               |
| `hdf5_dataset_exists(ifile, group)`           | check if specific dataset exists     |
| `hdf5_get_dimensions(ifile, dataset)`         | get number of dimensions             |
| `hdf5_get_shape(ifile, dataset, shape_array)` | get shape in form of an array        |

The dataset functions work in the same way as the group functions. One can create datasets with a full
(unix-like) path where, again, the non-existent parent groups are created on the fly.
We provide wrappers for the following datatypes for datasets:

* `logical`
* `integer`
* `real(4)`
* `real(8)`
* `complex(4)`
* `complex(8)`

The in/output is supported for all possible `0D` to `7D` (Fortran maximum) arrays. The readin functions
work the same way, only that we have to provide an allocatable array with the matching dimensionality instead.
Please note that there is neither a check for matching datatypes nor a check for matching dimensions.
From the above list HDF5 natively supports only `integer`, `real(4)`, and `real(8)`. `logical`, `complex(4)`,
and `complex(8)` on the other hand must be constructed manually. In order to achieve maximum compatibility
with `h5py` ([supported datatypes](https://docs.h5py.org/en/stable/faq.html#what-datatypes-are-supported)) I employed the identical structures:

| hdf5 datatype   | internal                                      |
| --------------- | --------------------------------------------- |
| `logical`       | HDF5 enum (0: FALSE, 1: TRUE) - h5t_native_b8 |
| `integer`       | h5t_native_integer                            |
| `real(4)`       | h5t_native_real                               |
| `real(8)`       | h5t_native_double                             |
| `complex(4)`    | HDF5 struct ("r", "i") - h5t_native_real      |
| `complex(8)`    | HDF5 struct ("r", "i") - h5t_native_double    |

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
	write(*,*) hdf5_data_exists(ifile, '/group1/group2/dataset')

	call hdf5_read_data(ifile, '/group1/group2/dataset', b)

	! same as for the groups
	call hdf5_list_datasets(ifile, '/group1/group2', list)
	ngroups = hdf5_get_number_datasets(ifile, '/group1/group2') ! 1

	! dimensions and shape
	call hdf5_get_shape(ifile, '/group1/group2/dataset', hdf_shape)
	dimensions = hdf5_get_dimensions(ifile, '/group1/group2/dataset')

	call hdf5_close_file(ifile)
	call hdf5_finalize()
end program
```


## Attributes

| hdf5 command                                                  | description                          |
| ------------------------------------------------------------- | ------------------------------------ |
| `hdf5_write_attribute(ifile, location, attr_name, attribute)` | write attribute                      |
| `hdf5_read_attribute(ifile, location, attr_name, variable)`   | read attribute                       |
| `hdf5_attribute_exists(ifile, location, attr_name)`           | check if specific attribute exists   |
| `hdf5_get_number_attributes(ifile, location)`                 | get number of attributes             |
| `hdf5_list_attributes(ifile, location, list)`                 | get list of attributes               |

We also provide a small interface for reading and writing attributes.
The following datatypes are supported:

* `logical`
* `integer`
* `real(4)`
* `real(8)`
* `complex(4)`
* `complex(8)`
* `character(len=*)`

Attributes can be attached to both groups and datasets. For this reason
we forgo automatic parent group creation. Any non-existance of objects will trigger an error.
Please note that the written strings will be byte strings within Python.

```
program attributes
	use hdf5
	use hdf5_wrapper

	integer    :: x = 3
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

## Deletion

| hdf5 command                                         | description                          |
| ---------------------------------------------------- | ------------------------------------ |
| `hdf5_delete(ifile, location)`                       | delete group/dataset                 |
| `hdf5_delete_attribute(ifile, location, attr_name)`  | delete attribute                     |

Deletion of datasets and groups is internally done simply by unlinking the objects.

## Summary of commands

| command                                                       | description                          | function      |
| ------------------------------------------------------------- | ------------------------------------ | :--------:    |
| `hdf5_init()`                                                 | initialize hdf5 interface            | -             |
| `hdf5_finalize()`                                             | close hdf5 interface                 | -             |
| `hdf5_create_file(fname)`                                     | create hdf5 file                     | -             |
| `hdf5_open_file(fname, ifile [,rdonly])`                      | open hdf5 file                       | -             |
| `hdf5_close_file(ifile)`                                      | close hdf5 file                      | -             |
| `hdf5_create_group(ifile, gname)`                             | create group(s) (parents included)   | -             |
| `hdf5_list_groups(ifile, gname)`                              | get list of groups                   | -             |
| `hdf5_get_number_groups(ifile, gname)`                        | get number of lists                  | yes (integer) |
| `hdf5_group_exists(ifile, gname)`                             | check if group exists                | yes (logical) |
| `hdf5_read_data(ifile, dname, adata)`                         | read data from dataset               | -             |
| `hdf5_write_data(ifile, dname, data)`                         | write datasets from arrays           | -             |
| `hdf5_dataset_exists(ifile, group)`                           | check if dataset exists              | yes (logical) |
| `hdf5_list_datasets(ifile, group, list)`                      | get list of datasets                 | -             |
| `hdf5_get_number_datasets(ifile, group)`                      | get number of datasets               | yes (integer) |
| `hdf5_get_dimensions(ifile, dataset)`                         | get number of dimensions             | yes (integer) |
| `hdf5_get_shape(ifile, dataset, shape_array)`                 | get shape in form of an array        | -             |
| `hdf5_write_attribute(ifile, location, attr_name, attribute)` | write attribute                      | -             |
| `hdf5_read_attribute(ifile, location, attr_name, variable)`   | read attribute                       | -             |
| `hdf5_attribute_exists(ifile, location, attr_name)`           | check if attribute exists            | yes (logical) |
| `hdf5_get_number_attributes(ifile, location)`                 | get number of attributes             | yes (integer) |
| `hdf5_list_attributes(ifile, location, list)`                 | get list of attributes               | -             |
| `hdf5_delete(ifile, location)`                                | delete group/dataset                 | -             |
| `hdf5_delete_attribute(ifile, location, attr_name)`           | delete attribute                     | -             |
