# Project Skills Inventory

This document serves as an index of project-specific skills and standards.

## [Copybook Standards](copybook_standards.md)
*   **Description**: Standardized structure for `.cpy` files (headers, level numbers, column alignment).
*   **Key Rules**:
    *   Header starts at **Column 7**.
    *   Level 05 starts at **Column 12**.
    *   **No Level 01** in copybooks.
    *   **Maintenance Tracking**: 6-byte Tag (`[Initials][YY][MM]`) in Columns 1-6 for all changes.

## [COBOL Program Standards](programa_standards.md)
*   **Description**: Mandatory alignment and coding patterns for `.cbl` files, following context-aware Cost Centers.
*   **Key Rules**:
    *   **Column 40 Alignment**: Mandatory for `DIVISION`, `SECTION`, `PIC`, `TO`, `INTO` and `EXIT`.
    *   **Maintenance Tracking**: 6-byte Tag (`[Initials][YY][MM]`) in Columns 1-6 for all changes.
    *   `EXIT` on the same line as parágrafo `-99-FIM`.
    *   Use of `SECTION` for modularity.

## [Mainframe Optimization](mainframe_skills_guide.md)
*   **Description**: Comprehensive guide on JCL, COBOL, DB2, CICS, and IMS best practices.
*   **Key Techniques**:
    *   DFSORT `OUTFIL` and `JOINKEYS`.
    *   COBOL `EVALUATE` and Intrinsic Functions.
    *   DB2 Multi-Row Fetch.

## Agent Skills (`.agent/skills/`)

Skills técnicas para o agente de IA, com base em melhores práticas atuais (web e documentação IBM/Broadcom).

| Skill | Descrição |
| :--- | :--- |
| **[dclgen_interpretation](../.agent/skills/dclgen_interpretation/SKILL.md)** | Interpretação de copybooks DCLGEN (DB2) em COBOL, tipos e estruturas. |
| **[ims_dc_online](../.agent/skills/ims_dc_online/SKILL.md)** | IMS/DC: programas online COBOL, chamadas DL/I (GU, GN, GNP), I/O PCB, processamento de mensagens e MFS. |
| **[cics_bms_maps](../.agent/skills/cics_bms_maps/SKILL.md)** | CICS transacional: BMS (DFHMSD, DFHMDI, DFHMDF), SEND/RECEIVE MAP, HANDLE CONDITION e copybooks de mapa. |
| **[jcl_batch](../.agent/skills/jcl_batch/SKILL.md)** | JCL batch: JOB/EXEC/DD, DSN, DISP, SPACE, DCB, utilitários (IEBGENER, IEBCOPY, IDCAMS, SORT) e boas práticas. |
