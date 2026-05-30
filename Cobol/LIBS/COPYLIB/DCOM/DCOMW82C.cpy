      *----------------------------------------------------------------*
      *                      Inicio da Geracao                         *
      *              DICM-CopyBook-Processo Publicacao                 *
      *----------------------------------------------------------------*
      * Copy Book - DCOMW82C
      * Data/Hora : 13/Dez/2017 14:19:28
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                       Inicio do Include                        *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
       05 DCOMW82A-OCORRENCIA
           OCCURS 1 TO 4 TIMES
           DEPENDING ON DCOMW82A-NOCOR.
          10 DCOMW82A-CD-LANC                                   PIC X(8)
              VALUE 'DC01L'.
          10 DCOMW82A-NOME-AVAL                               PIC X(40).
          10 DCOMW82A-CPFPJ-AVAL                              PIC X(19).
          10 DCOMW82A-IPSSOA-CONJG                            PIC X(40).
          10 DCOMW82A-CPFPJ-CONJG                             PIC X(19).
          10 DCOMW82A-NOME-TEST                               PIC X(40).
          10 DCOMW82A-CPFPF-TEST                              PIC X(19).
      *----------------------------------------------------------------*
      *                       Final do Include                         *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                       Final da Geracao                         *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
