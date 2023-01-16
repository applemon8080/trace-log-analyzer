use crate::address::CpuMemoryAddress;

#[derive(Debug, Eq, PartialEq)]
pub enum CpuMemoryAccessEventType {
    Read,
    Write,
}

#[derive(Debug, Eq, PartialEq)]
pub struct CpuMemoryAccessEvent {
    access_type: CpuMemoryAccessEventType,
    cpu_memory_address: CpuMemoryAddress,
    value: u8,
}

impl CpuMemoryAccessEvent {
    pub fn new(
        access_type: CpuMemoryAccessEventType,
        cpu_memory_address: CpuMemoryAddress,
        value: u8,
    ) -> CpuMemoryAccessEvent {
        CpuMemoryAccessEvent {
            access_type: access_type,
            cpu_memory_address: cpu_memory_address,
            value: value,
        }
    }

    pub fn access_type(&self) -> &CpuMemoryAccessEventType {
        &self.access_type
    }
    pub fn cpu_memory_address(&self) -> &CpuMemoryAddress {
        &self.cpu_memory_address
    }
    pub fn value(&self) -> &u8 {
        &self.value
    }
}
