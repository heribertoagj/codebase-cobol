      *----------------------------------------------------------------*
      *                      Inicio da Geracao                         *
      *              DICM-CopyBook-Processo Publicacao                 *
      *----------------------------------------------------------------*
      * Copy Book - DCOMW88C
      * Data/Hora : 19/Dez/2017 13:51:48
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                       Inicio do Include                        *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
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
