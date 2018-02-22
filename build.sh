#!/bin/bash
# sml < main-export.sml
mlton -prefer-abs-paths true -show-def-use celf.du celf.mlb
