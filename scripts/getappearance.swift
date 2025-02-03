#!/usr/bin/env swift
// Copyright 2025 Charles Y. Choi
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
