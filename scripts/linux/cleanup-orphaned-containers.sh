#!/bin/bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

# Clean up orphaned beamtalk devcontainers and volumes.
#
# Usage:
#   ./cleanup-orphaned-containers.sh [--all] [--dry-run]
#
# Options:
#   --all      Remove all beamtalk containers, even for active worktrees
#   --dry-run  Show what would be removed without actually removing

set -e

ALL=false
DRY_RUN=false

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --all)
            ALL=true
            shift
            ;;
        --dry-run)
            DRY_RUN=true
            shift
            ;;
        -h|--help)
            echo "Usage: $0 [--all] [--dry-run]"
            echo ""
            echo "Options:"
            echo "  --all      Remove all beamtalk containers, even for active worktrees"
            echo "  --dry-run  Show what would be removed without actually removing"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
GRAY='\033[0;90m'
WHITE='\033[1;37m'
NC='\033[0m'

log_info() { echo -e "${CYAN}$1${NC}"; }
log_success() { echo -e "${GREEN}$1${NC}"; }
log_warn() { echo -e "${YELLOW}$1${NC}"; }
log_error() { echo -e "${RED}$1${NC}"; }
log_gray() { echo -e "${GRAY}$1${NC}"; }

log_info "🐳 Beamtalk Container Cleanup"
echo ""

# Get list of active worktree paths
declare -a ACTIVE_WORKTREES
if [ "$ALL" = false ]; then
    log_info "📂 Finding active worktrees..."
    while IFS= read -r line; do
        if [[ $line =~ ^worktree\ (.+) ]]; then
            worktree_path="${BASH_REMATCH[1]}"
            ACTIVE_WORKTREES+=("$worktree_path")
            log_gray "   ✓ $worktree_path"
        fi
    done < <(git worktree list --porcelain 2>/dev/null)
    echo ""
fi

# Find all beamtalk-related containers
log_info "🔍 Scanning Docker containers..."
declare -a BEAMTALK_CONTAINERS
declare -a ORPHANED_CONTAINERS

while IFS='|' read -r id name image status; do
    # Check if this is a beamtalk container
    if [[ $image =~ beamtalk|vsc-bt- ]]; then
        BEAMTALK_CONTAINERS+=("$id|$name|$status")
        
        # Check if it's orphaned (if not running --all mode)
        if [ "$ALL" = false ]; then
            label=$(docker inspect --format '{{index .Config.Labels "devcontainer.local_folder"}}' "$id" 2>/dev/null || echo "")
            
            is_orphaned=true
            if [ -n "$label" ]; then
                for worktree in "${ACTIVE_WORKTREES[@]}"; do
                    if [[ $label == *"$worktree"* ]]; then
                        is_orphaned=false
                        break
                    fi
                done
            fi
            
            if [ "$is_orphaned" = true ]; then
                ORPHANED_CONTAINERS+=("$id|$name|$status")
            fi
        fi
    fi
done < <(docker ps -a --format "{{.ID}}|{{.Names}}|{{.Image}}|{{.Status}}" 2>/dev/null)

# Determine which containers to remove
if [ "$ALL" = true ]; then
    CONTAINERS_TO_REMOVE=("${BEAMTALK_CONTAINERS[@]}")
else
    CONTAINERS_TO_REMOVE=("${ORPHANED_CONTAINERS[@]}")
fi

echo -e "${WHITE}Found:${NC}"
log_gray "   Total beamtalk containers: ${#BEAMTALK_CONTAINERS[@]}"
if [ "$ALL" = false ]; then
    active_count=$((${#BEAMTALK_CONTAINERS[@]} - ${#ORPHANED_CONTAINERS[@]}))
    log_success "   Active worktree containers: $active_count"
    log_warn "   Orphaned containers: ${#ORPHANED_CONTAINERS[@]}"
fi
echo ""

if [ ${#CONTAINERS_TO_REMOVE[@]} -eq 0 ]; then
    log_success "✨ No containers to remove!"
    exit 0
fi

# Display what will be removed
log_warn "Will remove the following containers:"
for container in "${CONTAINERS_TO_REMOVE[@]}"; do
    IFS='|' read -r id name status <<< "$container"
    if [[ $status == *"Up"* ]]; then
        echo -e "   ${RED}[$status]${NC} ${WHITE}$name${NC}"
    else
        echo -e "   ${GRAY}[$status]${NC} ${WHITE}$name${NC}"
    fi
done
echo ""

if [ "$DRY_RUN" = true ]; then
    log_info "🔍 DRY RUN - No changes made"
    exit 0
fi

# Confirm before removing
read -p "Remove these containers? (y/N) " confirm
if [ "$confirm" != "y" ] && [ "$confirm" != "Y" ]; then
    log_error "❌ Cancelled"
    exit 0
fi

# Remove containers
echo ""
log_info "🗑️  Removing containers..."
removed=0
for container in "${CONTAINERS_TO_REMOVE[@]}"; do
    IFS='|' read -r id name status <<< "$container"
    
    log_gray "   Stopping: $name"
    docker stop "$id" 2>/dev/null || true
    
    log_gray "   Removing: $name"
    docker rm "$id" 2>/dev/null || true
    ((removed++))
done
log_success "✅ Removed $removed containers"

# Find and remove orphaned volumes
echo ""
log_info "🔍 Scanning for orphaned volumes..."
declare -a BEAMTALK_VOLUMES

while IFS= read -r volume; do
    if [[ $volume =~ bt-[0-9]+|beamtalk ]]; then
        BEAMTALK_VOLUMES+=("$volume")
    fi
done < <(docker volume ls --format "{{.Name}}" 2>/dev/null)

if [ ${#BEAMTALK_VOLUMES[@]} -gt 0 ]; then
    log_gray "Found ${#BEAMTALK_VOLUMES[@]} beamtalk-related volumes"
    
    read -p "Remove unused volumes? (y/N) " confirm_volumes
    if [ "$confirm_volumes" = "y" ] || [ "$confirm_volumes" = "Y" ]; then
        for volume in "${BEAMTALK_VOLUMES[@]}"; do
            if docker volume rm "$volume" 2>/dev/null; then
                log_success "   ✓ Removed: $volume"
            else
                log_warn "   ⚠️ Skipped (in use): $volume"
            fi
        done
    fi
fi

echo ""
log_success "✨ Done!"
