      ************************** PROCWORK ******************************
      * NOME DA INC - GFCTWAFZ                                         *
      * DESCRICAO   - EXCLUSAO MASSIVA ADESAO CESTA SERVICOS           *
      *               AREA DE TRABALHO                                 *
      * DATA        - MARCO/2009                                       *
      * RESPONSAVEL - WAGNER  - PROCWORK                               *
      * LRECL       - 0700                                             *
      ************************** PROCWORK ******************************
      *
       01 REG-GFCTWAFZ.
          03 WAFZ-SOLICITA.
             05 WAFZ-CAGPTO-CTA             PIC S9(003) COMP-3.
             05 WAFZ-CSERVC-TARIF           PIC S9(005) COMP-3.
             05 WAFZ-HINCL-REG              PIC  X(026).
             05 WAFZ-CFUNC-SOLTC-CANCT      PIC S9(009) COMP-3.
             05 WAFZ-CFUNC-MANUT            PIC S9(009) COMP-3.
             05 WAFZ-CSIT-SOLTC-CANCT       PIC S9(002) COMP-3.
             05 WAFZ-QPRVIO-CANCT-ADSAO     PIC S9(011) COMP-3.
             05 WAFZ-QADSAO-CANCD           PIC S9(011) COMP-3.
             05 WAFZ-HEXCUC-CALC-PRVIO      PIC  X(026).
             05 WAFZ-HEXCUC-PROCM           PIC  X(026).
             05 WAFZ-HMANUT-REG             PIC  X(026).
             05 WAFZ-RJUSTF-SOLTC-CANCT.
                07 WAFZ-RJUSTF-SOLTC-C-LEN  PIC S9(004) COMP.
                07 WAFZ-RJUSTF-SOLTC-C-TEXT PIC  X(256).
             05 WAFZ-RJUSTF-PAREC-CANCT.
                07 WAFZ-RJUSTF-PAREC-C-LEN  PIC S9(004) COMP.
                07 WAFZ-RJUSTF-PAREC-C-TEXT PIC  X(256).
          03 WAFZ-DETALHES.
             05 WAFZ-AGENCIA                PIC S9(005) COMP-3.
             05 WAFZ-POSTO                  PIC S9(005) COMP-3.
             05 WAFZ-CONTA                  PIC S9(013) COMP-3.
             05 WAFZ-CNPJ-NUM               PIC S9(009) COMP-3.
             05 WAFZ-CNPJ-FIL               PIC S9(005) COMP-3.
ST2506       05 WAFZ-CNPJ-NUM-ST            PIC X(009).
ST2506       05 WAFZ-CNPJ-FIL-ST            PIC X(004).
             05 WAFZ-CNPJ-CTR               PIC S9(002) COMP-3.
          03 FILLER                         PIC  X(015).
