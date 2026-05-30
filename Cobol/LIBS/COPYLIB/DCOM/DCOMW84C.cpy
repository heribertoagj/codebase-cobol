      *----------------------------------------------------------------*
      *                      Inicio da Geracao                         *
      *              DICM-CopyBook-Processo Publicacao                 *
      *----------------------------------------------------------------*
      * Copy Book - DCOMW84C
      * Data/Hora : 13/Dez/2017 14:21:08
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                       Inicio do Include                        *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
       05 DCOMW84A-OCORRENCIA
           OCCURS 1 TO 4 TIMES
           DEPENDING ON DCOMW84A-NOCOR.
          10 DCOMW84A-CD-LANC                                   PIC X(8)
              VALUE 'DC03L'.
          10 DCOMW84A-NOME-AVAL                               PIC X(40).
          10 DCOMW84A-CPFPJ-AVAL                              PIC X(19).
          10 DCOMW84A-NOME-TEST                               PIC X(40).
          10 DCOMW84A-CPFPJ-TEST                              PIC X(19).
      *----------------------------------------------------------------*
      *                       Final do Include                         *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                       Final da Geracao                         *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
