# Changelog

**v1.5.0:**

* Updated `ottr::export` so that notebook PDF generation failures do not prevent zip file creation [#766](https://github.com/ucbds-infra/otter-grader/issues/766)
* Added the `debug` argument to `ottr::export` to assist in debugging PDF generation failures
* Updated `ottr::export` force-save functionality to rely on [`ottr_force_save_labextension`](https://github.com/chrispyles/ottr-force-save-labextension) per [#767](https://github.com/ucbds-infra/otter-grader/issues/767)

**v1.4.0:**

* Added ability to save notebooks with `ottr::export` per [`ucbds-infra/otter-grader`#474](https://github.com/ucbds-infra/otter-grader/issues/474)

**v1.3.1:**

* Fixed bug in generating PDFs of RMarkdown files in `ottr::export`

**v1.3.0:**

* Added support for exporting PDFs in zip files created by `ottr:export`

**v1.2.0:**

* Refactored structure of the package
* Added/fixed documentation for objects and functions
* Added unit tests and CI
