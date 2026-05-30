      *----------------------------------------------------------------*
       01 FILLER                        PIC  X(032)    VALUE
             '*** AREA DA RUECT798 - INPUT ***'.
      *----------------------------------------------------------------*

       01  WRK-INPUT.
           03 FILLER                   PIC  X(018)         VALUE SPACES.
           03 WRK-I-PFK                 PIC  X(001)
                                                           VALUE SPACES.
           03 WRK-I-SENHAS              PIC  X(037)
                                                           VALUE SPACES.
           03 WRK-I-PAGINA              PIC  9(004)         VALUE ZEROS.
           03 WRK-I-FIMPAG              PIC  X(001)
                                                           VALUE SPACES.
           03 WRK-I-CTERM               PIC  X(008)
                                                           VALUE SPACES.
           03 WRK-I-FASE                PIC  9(001)         VALUE ZEROS.
           03 WRK-I-CHVOPE              PIC  9(007)         VALUE ZEROS.
           03 WRK-I-REFBACEN            PIC  X(007)
                                                           VALUE SPACES.
           03 WRK-I-DADOS.
              05 WRK-I-CINDPC-LST         OCCURS 12.
                 07 WRK-I-CINDPC        PIC  X(001)
                                                           VALUE SPACES.
                 07 WRK-I-DTVENC        PIC  X(010)
                                                           VALUE SPACES.
                 07 WRK-I-SLDPRI.
                    10 WRK-I-SLDPRI-N   PIC  9(013)         VALUE ZEROS.
                 07 WRK-I-SLDENC.
                    10 WRK-I-SLDENC-N   PIC  9(013)         VALUE ZEROS.
                 07 WRK-I-SLDTOT.
                    10 WRK-I-SLDTOT-N   PIC  ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
              05 WRK-I-MENSA            PIC  X(079)
                                                           VALUE SPACES.

      *----------------------------------------------------------------*
       01 FILLER                        PIC  X(032)    VALUE
             '*** AREA RUECT798  - OUTPUT  ***'.
      *----------------------------------------------------------------*

       01  WRK-OUTPUT.
           05 WRK-O-LL                 PIC S9(004) COMP    VALUE ZEROS.
           05 WRK-O-ZZ                 PIC S9(004) COMP    VALUE ZEROS.
           03 WRK-O-SENHAS              PIC  X(037)
                                                           VALUE SPACES.
           03 WRK-O-PAGINA              PIC  9(004)         VALUE ZEROS.
           03 WRK-O-FIMPAG              PIC  X(001)
                                                           VALUE SPACES.
           03 WRK-O-CTERM               PIC  X(008)
                                                           VALUE SPACES.
           03 WRK-O-FASE                PIC  9(001)         VALUE ZEROS.
           03 WRK-O-CHVOPE              PIC  9(007)         VALUE ZEROS.
           03 WRK-O-REFBACEN            PIC  X(007)
                                                           VALUE SPACES.
           03 WRK-O-DADOS.
              05 WRK-O-CINDPC-LST         OCCURS 12.
                 07 WRK-O-CINDPC-ATTR   PIC  9(04) COMP
                                                           VALUE ZEROS.
                 07 WRK-O-CINDPC        PIC  X(001)
                                                           VALUE SPACES.
                 07 WRK-O-DTVENC        PIC  X(010)
                                                           VALUE SPACES.
                 07 WRK-O-SLDPRI.
                    10 WRK-O-SLDPRI-N   PIC  9(013)         VALUE ZEROS.
                 07 WRK-O-SLDENC.
                    10 WRK-O-SLDENC-N   PIC  9(013)         VALUE ZEROS.
                 07 WRK-O-SLDTOT.
                    10 WRK-O-SLDTOT-N   PIC  ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
              05 WRK-O-MENSA            PIC  X(079)
                                                           VALUE SPACES.

      *----------------------------------------------------------------*
       01 FILLER                        PIC  X(032)    VALUE
             '*** AREA RUECT798 - BRAD0660 ***'.
      *----------------------------------------------------------------*

       01  WRK-660-RUECT798.
           03 WRK-660-LL             PIC S9(004) COMP    VALUE ZEROS.
           03 WRK-660-LL-OUTPUT      PIC S9(004) COMP    VALUE ZEROS.
           03 WRK-660-SENHAS         PIC S9(004) COMP    VALUE 0037.
           03 WRK-660-PAGINA         PIC S9(004) COMP    VALUE 0004.
           03 WRK-660-FIMPAG         PIC S9(004) COMP    VALUE 0001.
           03 WRK-660-CTERM          PIC S9(004) COMP    VALUE 0008.
           03 WRK-660-FASE           PIC S9(004) COMP    VALUE 0001.
           03 WRK-660-CHVOPE         PIC S9(004) COMP    VALUE 0007.
           03 WRK-660-REFBACEN       PIC S9(004) COMP    VALUE 0007.
           03 WRK-660-CINDPC-LST     OCCURS 12.
              05 WRK-660-CINDPC      PIC S9(004) COMP    VALUE 2003.
              05 WRK-660-DTVENC      PIC S9(004) COMP    VALUE 0010.
              05 WRK-660-SLDPRI      PIC S9(004) COMP    VALUE 0013.
              05 WRK-660-SLDENC      PIC S9(004) COMP    VALUE 0013.
              05 WRK-660-SLDTOT      PIC S9(004) COMP    VALUE 0014.
           03 WRK-660-MENSA          PIC S9(004) COMP    VALUE 0079.
