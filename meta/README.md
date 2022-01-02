Developer Notes
===============

Release Checklist
-----------------

  - Update version number in .emacs.
  - Update year in LICENSE.md.
  - Update CHANGES.md.
  - Add new screenshot (120x32) if necessary.
  - Commit changes.

    ```
    git add -p
    git commit
    ```

  - Tag the release.

    ```
    VER=

    git commit -em "Set version to $VER"
    git tag $VER -m "Emacs4CL $VER"
    git push origin main $VER
    ```