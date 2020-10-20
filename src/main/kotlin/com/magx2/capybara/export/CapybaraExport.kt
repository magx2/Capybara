package com.magx2.capybara.export

import com.magx2.capybara.export.python.CompileUnitToExport


interface CapybaraExport {
    fun export(units: Set<CompileUnitToExport>)
}