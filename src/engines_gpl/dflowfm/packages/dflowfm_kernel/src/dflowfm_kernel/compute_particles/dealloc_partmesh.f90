!> deallocate particle mesh data
subroutine dealloc_partmesh()
   use m_partmesh
   implicit none

   if ( allocated(edge2node ) ) deallocate(edge2node )
   if ( allocated(edge2cell ) ) deallocate(edge2cell )
   if ( allocated(xnode     ) ) deallocate(xnode     )
   if ( allocated(ynode     ) ) deallocate(ynode     )
   if ( allocated(znode     ) ) deallocate(znode     )

   if ( allocated(xzwcell   ) ) deallocate(xzwcell   )
   if ( allocated(yzwcell   ) ) deallocate(yzwcell   )
   if ( allocated(zzwcell   ) ) deallocate(zzwcell   )
   if ( allocated(areacell  ) ) deallocate(areacell  )

   if ( allocated(dnn       ) ) deallocate(dnn       )
   if ( allocated(dnx       ) ) deallocate(dnx       )
   if ( allocated(dny       ) ) deallocate(dny       )
   if ( allocated(dnz       ) ) deallocate(dnz       )
   if ( allocated(w         ) ) deallocate(w         )

   if ( allocated(edge2link ) ) deallocate(edge2link )
!  if ( allocated(nod2cell  ) ) deallocate(nod2cell  )
   if ( allocated(cell2nod  ) ) deallocate(cell2nod  )

   if ( allocated(icell2edge) ) deallocate(icell2edge)
   if ( allocated(jcell2edge) ) deallocate(jcell2edge)


   return
end subroutine dealloc_partmesh
