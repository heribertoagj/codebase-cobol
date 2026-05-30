      *----------------------------------------------------------------*
      *                      Inicio da Geracao                         *
      *              DICM-CopyBook-Processo Publicacao                 *
      *----------------------------------------------------------------*
      * Copy Book - DCOMW88A
      * Data/Hora : 19/Dez/2017 13:52:27
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                       Inicio do Include                        *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
       05 DCOMW88A-CD-FRMUL                                     PIC X(8)
           VALUE 'DCOMW88A'.
       05 DCOMW88A-CBAR-01                                     PIC 9(9).
       05 DCOMW88A-CBAR-02                                     PIC 9(1).
       05 DCOMW88A-CBAR-03                                     PIC 9(6).
       05 DCOMW88A-NNOTA-PROMIS                               PIC 9(11).
       05 DCOMW88A-DVCTO-FNAL-OPER                            PIC X(10).
       05 DCOMW88A-VALOR-NOTA-PROMIS                          PIC 9(17).
       05 DCOMW88A-NOME-BANCO                                 PIC X(20).
       05 DCOMW88A-NOME-AGENCIA                               PIC X(20).
       05 DCOMW88A-VAL-OPER-EXT                              PIC X(200).
       05 DCOMW88A-NOME-EMINENTE                              PIC X(40).
       05 DCOMW88A-RG-EMITENTE                                PIC X(15).
       05 DCOMW88A-CNPJ-EMINENTE                               PIC 9(9).
       05 DCOMW88A-FILIAL-EMINENTE                             PIC 9(4).
       05 DCOMW88A-CTRL-EMINENTE                               PIC 9(2).
       05 DCOMW88A-END-EMINENTE                               PIC X(40).
       05 DCOMW88A-CCEP                                        PIC X(8).
       05 DCOMW88A-NOCOR                                       PIC 9(2).
       05 DCOMW88A-OCORRENCIA
           OCCURS 1 TO 4 TIMES
           DEPENDING ON DCOMW88A-NOCOR.
          10 DCOMW88A-CD-LANC                                   PIC X(8)
              VALUE 'DC06L'.
          10 DCOMW88A-NOME-AVAL                               PIC X(40).
          10 DCOMW88A-CDOCTO-ID-AVAL                          PIC X(15).
          10 DCOMW88A-CNPJ-AVAL                                PIC 9(9).
          10 DCOMW88A-FILIAL-AVAL                              PIC 9(4).
          10 DCOMW88A-CTRL-AVAL                                PIC 9(2).
          10 DCOMW88A-END-AVAL                                PIC X(40).
          10 DCOMW88A-CEP-AVAL                                 PIC 9(8).
          10 DCOMW88A-CPFPJ-AVAL                              PIC X(19).
          10 DCOMW88A-NOME-CNJG                               PIC X(40).
          10 DCOMW88A-CPFPJ-CNJG                              PIC X(19).
      *----------------------------------------------------------------*
      *                       Final do Include                         *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                       Final da Geracao                         *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
