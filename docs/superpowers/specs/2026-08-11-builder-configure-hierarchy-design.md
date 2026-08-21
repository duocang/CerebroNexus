# Builder Configure hierarchy and Extra material tables

## Goal

Make Configure read as one coherent task instead of a stack of competing pages. The user should understand what was imported, edit related settings together, name every supplementary table, understand the Viewer App option without internal contract terminology, and retain visible orientation in the four-step workflow.

## Scope

This change covers the Builder Configure workspace, the staged progress navigation, supplementary table naming, the frozen Review representation of those tables, and the Viewer Extra material selector. It does not add plot upload to Builder.

## Information hierarchy

Configure is the only level-two heading in the workspace. Its content is organized into four sibling sections with level-three headings:

1. **Import & Inspect** — imported format, dataset size, detected content, warnings, and blockers.
2. **Core settings** — Dataset name, Organism, and Viewer content configuration.
3. **Optional enhancements** — optional analyses, Extra material tables, and spatial alignment when applicable.
4. **Viewer app** — app creation, authentication, and capability guidance.

Each section uses one shared card treatment and consistent heading, introduction, and field spacing. Nested groups such as Viewer content use level-four headings or disclosure titles. Dataset name and select controls use the same control height, inline padding, label spacing, and focus treatment.

## Supplementary table flow

The attachment control is labelled **Add tables…** and accepts CSV and TSV files. After selection, every accepted file appears directly below the control as a persistent file row containing:

- original filename;
- file type and size;
- current validation state;
- a required **Table name** input;
- a Remove action.

The default Table name is derived from the filename. Names must be non-empty and unique within the dataset. Inline validation identifies an empty or duplicate name and Configure cannot advance to Review until every row is valid. Renaming changes the display name only and does not reread the file.

The frozen Review shows both the final Table name and the source filename. The final Table name becomes the choice shown by the Viewer under **Choose a table**.

Upload states are file-specific: Reading, Ready, or a retained error with a useful explanation and Remove action. A failed file is never silently omitted.

## Viewer Extra material selector

The existing Viewer data model supports Tables and Plots. Builder continues to author Tables only in this scope.

When a Viewer dataset contains only Tables, the category selector is omitted and the page goes directly to **Choose a table**. When legacy or externally authored content contains more than one supported category, including Plots, the category selector remains available. This preserves compatibility without presenting a meaningless one-option choice.

## Viewer App copy and alignment

The Configure control is labelled **Create a Viewer app**. The toggle, explanatory text, authentication settings, and capability state live in the same Viewer app section and share a left alignment.

Internal terms such as “privacy contract v1” are not shown in normal UI. When app creation is unavailable, the user sees:

> Viewer app creation isn’t available in this installation. You can still build CRB files.

Internal capability checks and exact diagnostic reasons remain available to code and tests; only the user-facing presentation is simplified.

## Readiness and staged progress

Configure readiness uses **Ready to review** as the primary message. Dataset count is secondary context rather than the headline.

The Upload, Configure, Review, Build progress navigation is sticky to the bottom of the right-hand workspace. It does not cover or span the dataset rail. The workspace reserves sufficient bottom space so the navigation cannot obscure its final controls. The existing global top bar remains the only sticky element at the top.

On narrow screens the progress navigation presents the current step number and label compactly. It must remain keyboard accessible, respect safe-area insets, avoid obscuring focused controls when the on-screen keyboard is present, and preserve reduced-motion behavior.

## State and data boundaries

Table upload parsing remains owned by the enhancements server. Display-name validation is a pure operation shared by Configure readiness and Review freezing. The stored table record retains the original filename separately from its display name.

Configure UI renders live editable state. Review and Build render only the frozen plan. A successful rename or removal invalidates an existing confirmation through the existing workflow identity path.

Viewer category presentation is derived from the categories actually present in the loaded dataset; it does not hard-code Builder assumptions.

## Error handling

- Empty or duplicate table names produce inline, actionable errors and block Continue.
- Parse failures retain the attempted filename and error until removal.
- Removing a failed or valid file removes only that row.
- App capability failure disables only Viewer App creation; CRB output remains available.
- Malformed or unknown Viewer Extra material categories remain fail-closed and do not remove the supported-category compatibility path.

## Verification

Automated coverage will include:

- table selection, persistent file rows, default names, rename, remove, empty-name and duplicate-name rejection;
- readiness blocking and frozen Review table name/source filename output;
- Viewer behavior for Tables-only and multi-category Tables/Plots datasets;
- simplified Viewer App copy and absence of internal contract wording in user-facing Configure UI;
- one H2 Configure heading, four sibling H3 sections, and valid nested heading order;
- equal text/select geometry and Dataset name inline padding;
- desktop and mobile sticky progress visibility, workspace-only width, bottom clearance, focus visibility, safe-area behavior, and reduced motion;
- regression coverage for Configure → Review → Build and back navigation.

## Non-goals

- Builder plot upload or plot authoring.
- Changes to the underlying privacy capability contract.
- Redesign of the generated Viewer’s Extra material table itself.
- Changes to unrelated Builder analyses or dataset import formats.
