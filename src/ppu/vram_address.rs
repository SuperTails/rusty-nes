bitfield! {
    #[derive(Copy, Clone, PartialEq, Eq)]
    pub struct VRAMAddress(u16);
    impl Debug;

    pub coarse_x, set_coarse_x: 4, 0;

    pub coarse_y, set_coarse_y: 9, 5;

    pub nametable, set_nametable: 11, 10;

    pub fine_y, set_fine_y: 14, 12;
}

impl VRAMAddress {}
