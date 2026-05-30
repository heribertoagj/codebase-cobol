      *----------------------------------------------------------------*
       01 FILLER                        PIC  X(032)    VALUE
             '*** AREA DA RUCAT797 - INPUT ***'.
      *----------------------------------------------------------------*

       01  WRK-INPUT.
           03 FILLER                    PIC  X(018)        VALUE SPACES.
           03 WRK-I-PFK                 PIC  X(001)
                                                           VALUE SPACES.
           03 WRK-I-NOPCAO              PIC  X(002)
                                                           VALUE SPACES.
           03 WRK-I-SENHAS              PIC  X(037)
                                                           VALUE SPACES.
           03 WRK-I-PROX-TELA           PIC  X(008)
                                                           VALUE SPACES.
           03 WRK-I-CTERM               PIC  X(008)
                                                           VALUE SPACES.
           03 WRK-I-ANTE-TELA           PIC  X(008)
                                                           VALUE SPACES.
           03 WRK-I-PROXPAG             PIC  X(001)
                                                           VALUE SPACES.
           03 WRK-I-PAGINA              PIC  9(002)         VALUE ZEROS.
           03 WRK-I-CCONTR              PIC  X(009)
                                                           VALUE SPACES.
           03 WRK-I-DADOS.
              05 WRK-I-AGENCIA.
                 07 WRK-I-AGENCIA-N     PIC  9(005)         VALUE ZEROS.
              05 WRK-I-CONTA.
                 07 WRK-I-CONTA-N       PIC  9(008)         VALUE ZEROS.
              05 WRK-I-REFBACEN-LST       OCCURS 8.
                 07 WRK-I-REFBACEN      PIC  X(011)
                                                           VALUE SPACES.
                 07 WRK-I-CHVOPE.
                    10 WRK-I-CHVOPE-N   PIC  9(009)         VALUE ZEROS.
                 07 WRK-I-SLDPRI.
                    10 WRK-I-SLDPRI-N   PIC  ZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
                 07 WRK-I-SLDENC.
                    10 WRK-I-SLDENC-N   PIC  ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
                 07 WRK-I-CODBAS.
                    10 WRK-I-CODBAS-N   PIC  9(002)         VALUE ZEROS.
                 07 WRK-I-SLDREN.
                    10 WRK-I-SLDREN-N   PIC  Z.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
              05 WRK-I-CNPJ-CPF         PIC  X(018)
                                                           VALUE SPACES.
              05 WRK-I-SLDTOT.
                 07 WRK-I-SLDTOT-N      PIC  ZZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
              05 WRK-I-MENSA            PIC  X(079)
                                                           VALUE SPACES.

      *----------------------------------------------------------------*
       01 FILLER                        PIC  X(032)    VALUE
             '*** AREA RUCAT797  - OUTPUT  ***'.
      *----------------------------------------------------------------*

       01  WRK-OUTPUT.
           05 WRK-O-LL                 PIC S9(004) COMP    VALUE ZEROS.
           05 WRK-O-ZZ                 PIC S9(004) COMP    VALUE ZEROS.
           03 WRK-O-NOPCAO              PIC  X(002)
                                                           VALUE SPACES.
           03 WRK-O-SENHAS              PIC  X(037)
                                                           VALUE SPACES.
           03 WRK-O-PROX-TELA           PIC  X(008)
                                                           VALUE SPACES.
           03 WRK-O-CTERM               PIC  X(008)
                                                           VALUE SPACES.
           03 WRK-O-ANTE-TELA           PIC  X(008)
                                                           VALUE SPACES.
           03 WRK-O-PROXPAG             PIC  X(001)
                                                           VALUE SPACES.
           03 WRK-O-PAGINA              PIC  9(002)         VALUE ZEROS.
           03 WRK-O-CCONTR              PIC  X(009)
                                                           VALUE SPACES.
           03 WRK-O-DADOS.
              05 WRK-O-AGENCIA.
                 07 WRK-O-AGENCIA-N     PIC  9(005)         VALUE ZEROS.
              05 WRK-O-CONTA.
                 07 WRK-O-CONTA-N       PIC  9(008)         VALUE ZEROS.
              05 WRK-O-REFBACEN-LST       OCCURS 8.
                 07 WRK-O-REFBACEN-ATTR PIC  9(04) COMP
                                                           VALUE ZEROS.
                 07 WRK-O-REFBACEN      PIC  X(011)
                                                           VALUE SPACES.
                 07 WRK-O-CHVOPE-ATTR   PIC  9(04) COMP
                                                           VALUE ZEROS.
                 07 WRK-O-CHVOPE.
                    10 WRK-O-CHVOPE-N   PIC  9(009)         VALUE ZEROS.
                 07 WRK-O-SLDPRI-ATTR   PIC  9(04) COMP
                                                           VALUE ZEROS.
                 07 WRK-O-SLDPRI.
                    10 WRK-O-SLDPRI-N   PIC  ZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
                 07 WRK-O-SLDENC-ATTR   PIC  9(04) COMP
                                                           VALUE ZEROS.
                 07 WRK-O-SLDENC.
                    10 WRK-O-SLDENC-N   PIC  ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
                 07 WRK-O-CODBAS-ATTR   PIC  9(04) COMP
                                                           VALUE ZEROS.
                 07 WRK-O-CODBAS.
                    10 WRK-O-CODBAS-N   PIC  9(002)         VALUE ZEROS.
                 07 WRK-O-SLDREN-ATTR   PIC  9(04) COMP
                                                           VALUE ZEROS.
                 07 WRK-O-SLDREN.
                    10 WRK-O-SLDREN-N   PIC  Z.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
              05 WRK-O-CNPJ-CPF         PIC  X(018)
                                                           VALUE SPACES.
              05 WRK-O-SLDTOT-ATTR      PIC  9(04) COMP
                                                           VALUE ZEROS.
              05 WRK-O-SLDTOT.
                 07 WRK-O-SLDTOT-N      PIC  ZZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
              05 WRK-O-MENSA            PIC  X(079)
                                                           VALUE SPACES.

      *----------------------------------------------------------------*
       01 FILLER                        PIC  X(032)    VALUE
             '*** AREA RUCAT797 - BRAD0660 ***'.
      *----------------------------------------------------------------*

       01  WRK-660-RUCAT797.
           03 WRK-660-LL             PIC S9(004) COMP    VALUE ZEROS.
           03 WRK-660-LL-OUTPUT      PIC S9(004) COMP    VALUE ZEROS.
           03 WRK-660-NOVOOPC        PIC S9(004) COMP    VALUE 0002.
           03 WRK-660-SENHAS         PIC S9(004) COMP    VALUE 0037.
           03 WRK-660-PRTELA         PIC S9(004) COMP    VALUE 0008.
           03 WRK-660-CTERM          PIC S9(004) COMP    VALUE 0008.
           03 WRK-660-ANTELA         PIC S9(004) COMP    VALUE 0008.
           03 WRK-660-PROXPAG        PIC S9(004) COMP    VALUE 0001.
           03 WRK-660-PAGINA         PIC S9(004) COMP    VALUE 0002.
           03 WRK-660-CCONTR         PIC S9(004) COMP    VALUE 0009.
           03 WRK-660-AGENCIA        PIC S9(004) COMP    VALUE 0005.
           03 WRK-660-CONTA          PIC S9(004) COMP    VALUE 0008.
           03 WRK-660-REFBACEN-LST   OCCURS 8.
              05 WRK-660-REFBACEN    PIC S9(004) COMP    VALUE 2013.
              05 WRK-660-CHVOPE      PIC S9(004) COMP    VALUE 2011.
              05 WRK-660-SLDPRI      PIC S9(004) COMP    VALUE 2019.
              05 WRK-660-SLDENC      PIC S9(004) COMP    VALUE 2016.
              05 WRK-660-CODBAS      PIC S9(004) COMP    VALUE 2004.
              05 WRK-660-SLDREN      PIC S9(004) COMP    VALUE 2018.
           03 WRK-660-CNPJCPF        PIC S9(004) COMP    VALUE 0018.
           03 WRK-660-SLDTOT         PIC S9(004) COMP    VALUE 2020.
           03 WRK-660-MENSA          PIC S9(004) COMP    VALUE 0079.
