      *================================================================*
      *  I#BVVEDW - MANUTENCAO DE DOMICILIO BANCARIO                   *
      *  TRAVAS DE DOMICILIO ATIVAS                                    *
      *----------------------------------------------------------------*
      *  DSNAME       : CB.BACC.VON.TRAVPATI                           *
      *  LRECL ARQUIVO: 300                                            *
      *  LRECL CHAVE  : 060                                            *
      *----------------------------------------------------------------*
      *  FORMATOS                                                      *
      *   DATA        : AAAAMMDD                                       *
      *   TIMESTAMP   : AAAA-MM-DDTHH:MM:SS                            *
      *================================================================*

       01  BVVEDW-TRAVPATI.
           05 BVVEDW-CHAVE.
              10 BVVEDW-CH-CNPJ-CPF            PIC  9(009).
              10 BVVEDW-CH-FILIAL              PIC  9(004).
              10 BVVEDW-CH-COD-BAND            PIC  9(003).
              10 BVVEDW-CH-FILLER              PIC  X(044).
           05 BVVEDW-NUM-IDENT-SCG             PIC  9(017).
           05 BVVEDW-TP-PSSOA                  PIC  X(001).
           05 BVVEDW-TP-PRODT                  PIC  X(001).
           05 BVVEDW-AG-CLI                    PIC  9(004).
           05 BVVEDW-CTA-CLI                   PIC  9(013).
           05 BVVEDW-PERC-GAR                  PIC  9(003)V99.
           05 BVVEDW-DTINI-VALIDD              PIC  9(008).
           05 BVVEDW-DTFIM-VALIDD              PIC  9(008).
           05 BVVEDW-TIMES-MANUT               PIC  X(020).
           05 BVVEDW-CTRL-CNPJ-CPF             PIC  9(002).
           05 BVVEDW-COD-PROP-ESQM             PIC  X(003).
           05 BVVEDW-VLR-MAX-RET               PIC  9(013)V99.
           05 FILLER                           PIC  X(143).
