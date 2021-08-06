program main
  implicit none
  ! ------------------------------------------------------ !
  ! --- [0] variables & constants                      --- !
  ! ------------------------------------------------------ !
  integer         , parameter   ::   LI = 41
  integer         , parameter   ::   LJ = 31
  integer         , parameter   ::   LK = 1
  integer         , parameter   :: cLen = 300
  integer         , parameter   ::  lun = 50
  double precision, parameter   :: xMin = -1.d0, xMax=+1.d0
  double precision, parameter   :: yMin = -1.d0, yMax=+1.d0
  double precision, parameter   :: zMin =  0.d0, zMax= 0.d0
  integer         , parameter   :: xp_=1, yp_=2, zp_=3, fp_=4
  integer                       :: i, j, k
  double precision              :: dx, dy, dz
  character(cLen)               :: binFile = "dat/out.bin"
  character(cLen)               :: cnfFile = "dat/parameter.conf"
  double precision, allocatable :: Data(:,:,:,:)

  ! ------------------------------------------------------ !
  ! --- [1] make sample profile                        --- !
  ! ------------------------------------------------------ !
  allocate( Data(4,LI,LJ,LK) )
  Data(:,:,:,:) = 0.d0

  dx = ( xMax - xMin ) / dble( LI-1 )
  dy = ( yMax - yMin ) / dble( LJ-1 )
  ! dz = ( zMax - zMin ) / dble( LK-1 )
  dz = 0.d0
  do k=1, LK
     do j=1, LJ
        do i=1, LI
           Data(xp_,i,j,k) = xMin + dx * float(i-1)
           Data(yp_,i,j,k) = yMin + dy * float(j-1)
           Data(zp_,i,j,k) = zMin + dz * float(k-1)
           Data(fp_,i,j,k) = sqrt( Data(xp_,i,j,k)**2 + Data(yp_,i,j,k)**2 )
        enddo
     enddo
  enddo

  ! ------------------------------------------------------ !
  ! --- [2] save in a file                             --- !
  ! ------------------------------------------------------ !
  open (lun,file=trim(binFile),status="replace",form="unformatted")
  write(lun) Data
  close(lun)

  open (lun,file=trim(cnfFile),status="replace",form="formatted")
  write(lun,"(a25,1x,a10,1x,i12)") "LI", "integer", LI
  write(lun,"(a25,1x,a10,1x,i12)") "LJ", "integer", LJ
  write(lun,"(a25,1x,a10,1x,i12)") "LK", "integer", LK
  close(lun)

end program main
