import subprocess
import sys
import os
import io

def run_p4_command(command_args):
    """
    Runs a p4 command and returns its stdout.
    Raises an exception if the command fails.
    """
    try:
        result = subprocess.run(
            ['p4'] + command_args,
            capture_output=True,
            text=True,
            check=True,
            encoding='utf-8',
            errors='replace' # Handle potential encoding issues
        )
        return result.stdout
    except subprocess.CalledProcessError as e:
        print(f"Error running p4 command: {' '.join(e.cmd)}", file=sys.stderr)
        print(f"Stderr: {e.stderr}", file=sys.stderr)
        sys.exit(1) # Exit with an error code

def get_file_content(file_path_at_rev):
    """Retrieves the content of a file at a specific Perforce revision."""
    try:
        return run_p4_command(['print', file_path_at_rev])
    except SystemExit: # Catch the exit from run_p4_command if 'p4 print' fails
        print(f"Warning: Could not get content for {file_path_at_rev}", file=sys.stderr)
        return ""

def generate_patch(changelist_id, context_lines=3):
    """
    Generates a patch string for a given Perforce changelist,
    handling adds, deletes, edits, and renames.
    """
    patch_output = io.StringIO()

    # 1. Get description and file details for the changelist
    desc_output = run_p4_command(['describe', '-S', str(changelist_id)])

    files_in_changelist = []
    # Parse the 'describe -S' output to get file paths and actions
    for line in desc_output.splitlines():
        if line.strip().startswith('//'):
            parts = line.strip().split(' - ', 1)
            if len(parts) == 2:
                file_rev_part = parts[0]
                action_info = parts[1]

                file_path = file_rev_part.split('#')[0]
                action = action_info.split(' ')[0].strip()

                from_path = None
                if '(from ' in action_info:
                    from_match = action_info.split('(from ')[1].split(')')[0]
                    from_path = from_match.split('#')[0]

                files_in_changelist.append({
                    'path': file_path,
                    'action': action,
                    'from_path': from_path
                })

    # 2. Iterate through files and generate diffs
    for file_info in files_in_changelist:
        file_path = file_info['path']
        action = file_info['action']
        from_path = file_info['from_path']

        # Determine the previous revision for the file (for edits/deletes/renames)
        try:
            prev_changes_output = run_p4_command(['changes', '-m1', f'{file_path}@<={int(changelist_id)-1}'])
            prev_cl = prev_changes_output.splitlines()[0].split(' ')[1] if prev_changes_output else None
        except (subprocess.CalledProcessError, IndexError):
            prev_cl = None

        prev_revision_spec = f'@{prev_cl}' if prev_cl else '#none'
        current_revision_spec = f'@{changelist_id}'


        if action == 'add' or action == 'branch':
            # For adds and branches, compare with /dev/null
            display_path = file_path.lstrip('//').replace('/', os.sep)

            diff_command = ['p4', 'diff', '-ud', str(context_lines), f'{file_path}#none', f'{file_path}{current_revision_spec}']
            diff_output = run_p4_command(diff_command)

            reformatted_diff = []
            for dline in diff_output.splitlines():
                if dline.startswith('--- '):
                    reformatted_diff.append('--- /dev/null')
                elif dline.startswith('+++ '):
                    reformatted_diff.append(f'+++ {display_path}')
                else:
                    reformatted_diff.append(dline)
            patch_output.write('\n'.join(reformatted_diff))
            patch_output.write('\n')


        elif action == 'delete':
            # For deletes, compare the file at the previous revision with /dev/null
            display_path = file_path.lstrip('//').replace('/', os.sep)

            diff_command = ['p4', 'diff', '-ud', str(context_lines), f'{file_path}{prev_revision_spec}', f'{file_path}#none']
            diff_output = run_p4_command(diff_command)

            reformatted_diff = []
            for dline in diff_output.splitlines():
                if dline.startswith('--- '):
                    reformatted_diff.append(f'--- {display_path}')
                elif dline.startswith('+++ '):
                    reformatted_diff.append('+++ /dev/null')
                else:
                    reformatted_diff.append(dline)
            patch_output.write('\n'.join(reformatted_diff))
            patch_output.write('\n')


        elif action == 'edit':
            # For edits, compare between prev_revision and current_revision
            diff_command = ['p4', 'diff', '-ud', str(context_lines),
                            f'{file_path}{prev_revision_spec}',
                            f'{file_path}{current_revision_spec}']
            diff_output = run_p4_command(diff_command)

            reformatted_diff = []
            for dline in diff_output.splitlines():
                if dline.startswith('--- '):
                    reformatted_diff.append('--- ' + dline[4:].split('#')[0].lstrip('//').replace('/', os.sep))
                elif dline.startswith('+++ '):
                    reformatted_diff.append('+++ ' + dline[4:].split('#')[0].lstrip('//').replace('/', os.sep))
                else:
                    reformatted_diff.append(dline)
            patch_output.write('\n'.join(reformatted_diff))
            patch_output.write('\n')

        elif action == 'move/add':
            display_path = file_path.lstrip('//').replace('/', os.sep)
            diff_command = ['p4', 'diff', '-ud', str(context_lines), f'{file_path}#none', f'{file_path}{current_revision_spec}']
            diff_output = run_p4_command(diff_command)
            reformatted_diff = []
            for dline in diff_output.splitlines():
                if dline.startswith('--- '):
                    reformatted_diff.append('--- /dev/null')
                elif dline.startswith('+++ '):
                    reformatted_diff.append(f'+++ {display_path}')
                else:
                    reformatted_diff.append(dline)
            patch_output.write('\n'.join(reformatted_diff))
            patch_output.write('\n')

        elif action == 'move/delete':
            display_path = file_path.lstrip('//').replace('/', os.sep)
            diff_command = ['p4', 'diff', '-ud', str(context_lines), f'{file_path}{prev_revision_spec}', f'{file_path}#none']
            diff_output = run_p4_command(diff_command)
            reformatted_diff = []
            for dline in diff_output.splitlines():
                if dline.startswith('--- '):
                    reformatted_diff.append(f'--- {display_path}')
                elif dline.startswith('+++ '):
                    reformatted_diff.append('+++ /dev/null')
                else:
                    reformatted_diff.append(dline)
            patch_output.write('\n'.join(reformatted_diff))
            patch_output.write('\n')


    return patch_output.getvalue()

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python generate_p4_patch.py <changelist_id> [context_lines]", file=sys.stderr)
        sys.exit(1)

    changelist = sys.argv[1]
    context = int(sys.argv[2]) if len(sys.argv) > 2 else 3

    try:
        patch_content = generate_patch(changelist, context)
        sys.stdout.write(patch_content)
    except Exception as e:
        print(f"An error occurred: {e}", file=sys.stderr)
        sys.exit(1)
