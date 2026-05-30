      *----------------------------------------------------------------*
       01 FILLER                        PIC  X(032)    VALUE
             '*** AREA DA XXXXT999 - INPUT ***'.
      *----------------------------------------------------------------*

       01  WRK-INPUT.
           03 FILLER                   PIC  X(018)         VALUE SPACES.
           03 WRK-I-PFK                 PIC  X(001)
                                                           VALUE SPACES.
           03 WRK-I-SENHAS              PIC  X(037)
                                                           VALUE SPACES.
           03 WRK-I-NOPCAO              PIC  X(002)
                                                           VALUE SPACES.
           03 WRK-I-PROX-TELA           PIC  X(008)
                                                           VALUE SPACES.
           03 WRK-I-ANTE-TELA           PIC  X(008)
                                                           VALUE SPACES.
           03 WRK-I-FASE                PIC  9(002)         VALUE ZEROS.
           03 WRK-I-CTERM               PIC  X(008)
                                                           VALUE SPACES.
           03 WRK-I-DADOS.
              05 WRK-I-SELITEM-LST        OCCURS 10.
                 07 WRK-I-SELITEM       PIC  X(001)
                                                           VALUE SPACES.
                 07 WRK-I-LSTCOD        PIC  X(010)
                                                           VALUE SPACES.
                 07 WRK-I-LSTDESC       PIC  X(030)
                                                           VALUE SPACES.
                 07 WRK-I-LSTVALOR      PIC  X(010)
                                                           VALUE SPACES.
              05 WRK-I-MENSA            PIC  X(079)
                                                           VALUE SPACES.

      *----------------------------------------------------------------*
       01 FILLER                        PIC  X(032)    VALUE
             '*** AREA XXXXT999  - OUTPUT  ***'.
      *----------------------------------------------------------------*

       01  WRK-OUTPUT.
           05 WRK-O-LL                 PIC S9(004) COMP    VALUE ZEROS.
           05 WRK-O-ZZ                 PIC S9(004) COMP    VALUE ZEROS.
           03 WRK-O-SENHAS              PIC  X(037)
                                                           VALUE SPACES.
           03 WRK-O-NOPCAO              PIC  X(002)
                                                           VALUE SPACES.
           03 WRK-O-PAGINA              PIC  9(002)         VALUE ZEROS.
           03 WRK-O-PROX-TELA           PIC  X(008)
                                                           VALUE SPACES.
           03 WRK-O-ANTE-TELA           PIC  X(008)
                                                           VALUE SPACES.
           03 WRK-O-FASE                PIC  9(002)         VALUE ZEROS.
           03 WRK-O-CTERM               PIC  X(008)
                                                           VALUE SPACES.
           03 WRK-O-DADOS.
              05 WRK-O-SELITEM-LST        OCCURS 10.
                 07 WRK-O-SELITEM-ATTR  PIC  9(04) COMP
                                                           VALUE ZEROS.
                 07 WRK-O-SELITEM       PIC  X(001)
                                                           VALUE SPACES.
                 07 WRK-O-LSTCOD        PIC  X(010)
                                                           VALUE SPACES.
                 07 WRK-O-LSTDESC       PIC  X(030)
                                                           VALUE SPACES.
                 07 WRK-O-LSTVALOR      PIC  X(010)
                                                           VALUE SPACES.
              05 WRK-O-MENSA            PIC  X(079)
                                                           VALUE SPACES.
