#!/usr/bin/env swift
// Copyright 2025 Charles Y. Choi

// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

import Foundation
import AppKit

func getOSAppearance () {
    let app = NSApplication.shared
    let appearance = app.effectiveAppearance

    switch appearance.name {
    case .darkAqua:
        print("dark")

    default:
        print("light")
    }
}

getOSAppearance()
