!! Copyright (c) 2018 William Matthew Peterson
!! Distributed under LGPLv3 License.
!! 
module FHDICT
    !! A Key-Value Dictionary implemented with a hashtable and singly-linked lists.
    !!
    !! Currently supports mapping integer keys to:
    !!  1) a variable length integer array (integer, dimension(:), allocatable)
    !!  2) an integer value                (integer)
    !!  3) a variable length real array    (real, dimension(:), allocatable)
    !!
    !! BASIC USAGE:
    !!  use fhdict                                  module access
    !!  type(hashtable) :: dict                     type/instance declaration
    !!  call dict%init([nitems=n, is_mutable=TF])   initialization
    !!  call dict%put(key=k, val=v [,rc=r])         puts (k,v) into hashtable
    !!  call dict%get(key=k, val=v [,rc=r])         gets v for a specified k
    !!  call dict%free()                            deallocates dictionary
    
    implicit none
    private
    public :: hashtable
    
    integer, dimension(8), parameter :: primes = [101, 503, 5001, 50021, 500009, &
                                                  1000003, 1500007, 2000003]
    
    type sllist
        type(sllist), pointer                   :: child => null()
        integer, allocatable                    :: key
        integer, allocatable                    :: ival
        integer, allocatable                    :: val(:)
        real(8), allocatable                    :: rvals(:)
    contains
        procedure, pass :: put_sll              !!
        procedure, pass :: get_sll              !!
        procedure, pass :: count_sll            !!
        procedure, pass :: has_key_sll          !!
        procedure, pass :: keys_sll             !!
        procedure, pass :: free_sll             !!
    end type sllist
    
    
    type hashtable
        type(sllist), dimension(:), allocatable :: tbl
        integer                                 :: capacity = 0
        integer                                 :: count = 0
        logical                                 :: is_init = .false.
        logical                                 :: is_mutable = .true.
    contains
        procedure, pass :: init                 !!
        procedure, pass :: put                  !!
        procedure, pass :: get                  !!
        procedure, pass :: del                  !!
        procedure, pass :: keys                 !!
        procedure, pass :: has_key              !!
        procedure, pass :: hash                 !!
        procedure, pass :: free                 !!
    end type hashtable
    
contains
    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! SINGLY-LINKED LIST METHODS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    recursive subroutine put_sll(list, key, val, ival, rvals, mutable, rc)
        class(sllist), intent(inout)      :: list
        integer, intent(in)               :: key
        integer, intent(in), optional     :: val(:)
        integer, intent(in), optional     :: ival
        real(8), intent(in), optional     :: rvals(:)
        logical, intent(in)               :: mutable
        integer, intent(out)              :: rc
        
        rc = 0
        
        if (allocated(list%key)) then
            !! The incoming key has the same hash as an existing key in the dict.
            if (list%key /= key) then
                !! Different keys. Extend the singly linked list.
                if ( .not. associated(list%child) ) allocate(list%child)
                
                if (present(val)) then
                    call put_sll(list=list%child, key=key, val=val, mutable=mutable, rc=rc)
                elseif (present(ival)) then
                    call put_sll(list=list%child, key=key, ival=ival, mutable=mutable, rc=rc)
                elseif (present(rvals)) then
                    call put_sll(list=list%child, key=key, rvals=rvals, mutable=mutable, rc=rc)
                endif
                
                if (rc) then
                    print *, "FHDICT: Collision detected."
                    print *, "existing key: ", list%key
                    print *, "new key     : ", key
                    print *, "Error while allocating sllist child."
                endif
            else
                !! Same keys. Overwrite if mutable.
                if (mutable) then
                    list%key = key
                    if (present(val)) then
                        if (allocated(list%val)) deallocate(list%val)
                        allocate(list%val, source=val)
                    elseif (present(ival)) then
                        list%ival = ival
                    elseif (present(rvals)) then
                        if (allocated(list%rvals)) deallocate(list%rvals)
                        allocate(list%rvals, source=rvals)
                    endif
                    rc = -1
                else
                    print *, ""
                    print *, "FHDICT: Conflict at key: ", list%key
                    print *, "The 'is_mutable=.false.' attribute has been set."
                    print *, "This prohibits overwriting existing key values."
                    rc = 1
                endif
            endif
        else
            allocate(list%key, source=key)
            if (present(val)) then
                if (allocated(list%val)) deallocate(list%val)
                allocate(list%val, source=val)
            elseif (present(ival)) then
                list%ival = ival
            elseif (present(rvals)) then
                if (allocated(list%rvals)) deallocate(list%rvals)
                allocate(list%rvals, source=rvals)
            endif
        endif
        
    end subroutine put_sll
    
    
    recursive subroutine get_sll(list, key, val, ival, rvals, rc)
        class(sllist), intent(in)                   :: list
        integer, intent(in)                         :: key
        integer, intent(out), allocatable, optional :: val(:)
        integer, intent(out), optional              :: ival
        real(8), intent(out), allocatable, optional :: rvals(:)
        integer, intent(out)                        :: rc
        
        rc = 0
        if ((allocated(list%key)) .and. (list%key == key)) then
            if (present(val)) then
                if (allocated(val)) deallocate(val)
                allocate(val, source=list%val)
            elseif (present(ival)) then
                ival = list%ival
            elseif (present(rvals)) then
                if (allocated(rvals)) deallocate(rvals)
                allocate(rvals, source=list%rvals)
            endif
        else if(associated(list%child)) then
            if (present(val)) then
                call get_sll(list=list%child, key=key, val=val, rc=rc)
            elseif (present(ival)) then
                call get_sll(list=list%child, key=key, ival=ival, rc=rc)
            elseif (present(rvals)) then
                call get_sll(list=list%child, key=key, rvals=rvals, rc=rc)
            endif
        else
            !! at the end of the list, no key found
            if (allocated(val)) deallocate(val)
            if (allocated(rvals)) deallocate(rvals)
            rc = 1
        end if
    end subroutine get_sll
    
    
    recursive subroutine has_key_sll(list, key, rc)
        class(sllist), intent(in)                       :: list
        integer, intent(in)                             :: key
        integer, intent(out)                            :: rc
        
        if ((allocated(list%key)) .and. (list%key == key)) then
            rc = 0
        elseif (associated(list%child)) then
            call has_key_sll(list%child,key,rc)
        else
            !! key not found
            rc = 1
        endif
    end subroutine has_key_sll
    
    
    recursive subroutine keys_sll(list, k, n)
        class(sllist), intent(in)                       :: list
        integer, dimension(:), intent(inout)            :: k
        integer, intent(out)                            :: n
        
        if (allocated(list%key)) then
            n = n+1
            k(n) = list%key
            if (associated(list%child)) then
                call keys_sll(list%child, k, n)
            endif
        endif
    end subroutine keys_sll
    
    
    recursive subroutine free_sll(list, rc)
        class(sllist), intent(inout)    :: list
        integer, intent(out)            :: rc
        
        if (associated(list%child)) then
            call free_sll(list%child,rc)
            deallocate(list%child, stat=rc)
        end if
        list%child => null()
        if (allocated(list%key)) deallocate(list%key, stat=rc)
        if (allocated(list%val)) deallocate(list%val, stat=rc)
        if (allocated(list%rvals)) deallocate(list%rvals, stat=rc)
    end subroutine free_sll
    
    
    recursive subroutine count_sll(list, n)
        class(sllist), intent(in)       :: list
        integer, intent(inout)          :: n
        
        if (associated(list%child)) then
            n = n+1
            call count_sll(list%child, n)
        endif
    end subroutine count_sll
    
    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! DICTIONARY/HASHTABLE METHODS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    integer function hash(dict, key) result(out)
        class(hashtable), intent(in)        :: dict
        integer, intent(in)                 :: key
        out = mod(key, dict%capacity) + 1
    end function hash
    
    
    subroutine init(dict, nitems, is_mutable)
        class(hashtable), intent(inout)     :: dict
        integer, optional, intent(in)       :: nitems
        logical, optional, intent(in)       :: is_mutable
        integer                             :: i, sz
        
        if (allocated(dict%tbl)) deallocate(dict%tbl)
        if (present(nitems)) then
            sz = nitems
            do i = 1,size(primes)
                if (sz <= primes(i)) then
                    sz = primes(i)
                    exit
                endif
            enddo
            
            allocate(dict%tbl(1:sz))
            dict%capacity = sz
        else
            sz = primes(1)
            allocate(dict%tbl(1:sz))
            dict%capacity = sz
        end if
        
        if (present(is_mutable)) then
            dict%is_mutable = is_mutable
        else
            dict%is_mutable = .true.
        endif
        dict%is_init = .true.
    end subroutine init
    
    
    logical function has_key(dict, key) result(found)
        class(hashtable), intent(in)        :: dict
        integer, intent(in)                 :: key
        integer                             :: hash, rc
        integer, dimension(:), allocatable  :: out
        
        found = .false.
        if (dict%count > 0) then
            hash = dict%hash(key)
            if (allocated(dict%tbl(hash)%key)) then
                call dict%tbl(hash)%has_key_sll(key=key, rc=rc)
                if (rc == 0) then
                    found = .true.
                endif
            endif
        endif
    end function has_key
    
    
    subroutine put(dict, key, val, ival, rvals, rc)
        class(hashtable), intent(inout)     :: dict
        integer, intent(in)                 :: key
        integer, intent(in), optional       :: val(:)
        integer, intent(in), optional       :: ival
        real(8), intent(in), optional       :: rvals(:)
        integer, optional, intent(out)      :: rc
        integer                             :: hash
        integer                             :: ret
        
        ret = 0
        hash = dict%hash(key)
        if (present(val)) then
            call dict%tbl(hash)%put_sll(key=key, val=val, mutable=dict%is_mutable, rc=ret)
        elseif (present(ival)) then
            call dict%tbl(hash)%put_sll(key=key, ival=ival, mutable=dict%is_mutable, rc=ret)
        elseif (present(rvals)) then
            call dict%tbl(hash)%put_sll(key=key, rvals=rvals, mutable=dict%is_mutable, rc=ret)
        endif
        if (ret == 0) then 
            dict%count = dict%count + 1
        elseif (ret < 0) then
            !! Overwrite an exising key. Do not increment count.
            ret = 0
        endif
        if (present(rc)) rc = ret
    end subroutine put
    
    
    subroutine get(dict, key, val, ival, rvals, rc)
        class(hashtable), intent(in)        :: dict
        integer, intent(in)                 :: key
        integer, intent(out), allocatable, optional :: val(:)
        integer, intent(out), optional      :: ival
        real(8), intent(out), allocatable, optional :: rvals(:)
        integer, intent(out), optional      :: rc
        integer                             :: hash
        integer                             :: ret
        
        ret = 0
        hash = dict%hash(key)
        if (dict%has_key(key)) then
            if (present(val)) then
                call dict%tbl(hash)%get_sll(key=key, val=val, rc=ret)
            elseif (present(ival)) then
                call dict%tbl(hash)%get_sll(key=key, ival=ival, rc=ret)
            elseif (present(rvals)) then
                call dict%tbl(hash)%get_sll(key=key, rvals=rvals, rc=ret)
            endif
        else
            print *, "Key not found: ", key
            ret = 1
        endif
        if (present(rc)) rc = ret
    end subroutine get
    
    
    subroutine keys(dict, k, rc)
        class(hashtable), intent(in)        :: dict
        integer, allocatable, intent(out)   :: k(:)
        integer, optional, intent(out)      :: rc
        integer                             :: i, n, hash
        integer                             :: ret
        
        ret = 0
        if (dict%count > 0) then
            if (allocated(k)) deallocate(k)
            allocate(k(dict%count))
            n = 0
            do i = 0, dict%capacity-1
                hash = dict%hash(i)
                call dict%tbl(hash)%keys_sll(k,n)
            enddo
        else
            ret = 1
        endif
        if (present(rc)) rc = ret
    end subroutine keys
    
    
    subroutine del(dict, key, rc)
        class(hashtable), intent(inout)     :: dict
        ! type(hashtable), intent(inout)     :: dict
        integer, intent(in)                 :: key
        integer, optional, intent(out)      :: rc
        integer                             :: hash
        integer                             :: ret, n, nkey, i, tempkey
        integer, allocatable                :: child_keys(:), val(:)
        type(hashtable)                     :: temp
        
        ret = 0
        hash = dict%hash(key)
        if (dict%has_key(key)) then
            n = 0
            call dict%tbl(hash)%count_sll(n)
            
            print *,     "del: child count, n   =", n
            if (n > 0) then
                !! Get list of keys stored in hash.
                allocate(child_keys(n+1))
                nkey = 0
                call dict%tbl(hash)%keys_sll(child_keys,nkey)
                
                !! Add child key/value pairs into temp dict.
                call temp%init(n)
                do i = 1,nkey
                    tempkey = child_keys(i)
                    if (tempkey == key) then
                        continue
                    else
                        call dict%get(key=tempkey, val=val, rc=ret)
                        if (ret /= 0) exit
                        call temp%put(key=tempkey, val=val, rc = ret)
                        if (ret /= 0) exit
                    endif
                enddo
            endif
            
            !! Remove the original key/values stored in the hash.
            call dict%tbl(hash)%free_sll(ret)
            
            !! Return child key/value pairs to original dict.
            if (n > 0) then
                if (ret == 0) then
                    dict%count = dict%count - 1
                    do i = 1,nkey
                        tempkey = child_keys(i)
                        if (tempkey == key) then
                            continue
                        else
                            call dict%get(key=tempkey, val=val, rc=ret)
                            if (ret /= 0) exit
                            call dict%put(key=tempkey, val=val, rc=ret)
                            if (ret /= 0) exit
                        endif
                    enddo
                endif
                !! Deallocate the temp dict
                call temp%free()
            endif
            if (ret == 0) dict%count = dict%count - 1
            
        else
            !! Key not in table.
            ret = -1
        endif
        
        if (present(rc)) rc = ret
    end subroutine del
    
    
    subroutine free(dict)
        class(hashtable), intent(inout)     :: dict
        integer                             :: i, lb, ub
        integer                             :: ret
        
        ret = 0
        lb = lbound(dict%tbl,dim=1)
        ub = ubound(dict%tbl,dim=1)
        if (allocated(dict%tbl)) then
            do i=lb,ub
                call dict%tbl(i)%free_sll(ret)
                if (ret == 0) dict%count = dict%count - 1
            end do
            if (ret == 0) deallocate(dict%tbl, stat=ret)
        end if
        if (ret == 0) then 
            dict%is_init = .false.
        endif
    end subroutine free
    
end module FHDICT
