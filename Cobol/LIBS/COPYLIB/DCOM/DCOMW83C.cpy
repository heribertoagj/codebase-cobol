      *----------------------------------------------------------------*
      *                      Inicio da Geracao                         *
      *              DICM-CopyBook-Processo Publicacao                 *
      *----------------------------------------------------------------*
      * Copy Book - DCOMW83C
      * Data/Hora : 13/Dez/2017 14:20:03
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                       Inicio do Include                        *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
       05 DCOMW83A-OCORRENCIA
           OCCURS 1 TO 4 TIMES
           DEPENDING ON DCOMW83A-NOCOR.
          10 DCOMW83A-CD-LANC                                   PIC X(8)
              VALUE 'DC02L'.
          10 DCOMW83A-NOME-AVAL                               PIC X(40).
          10 DCOMW83A-CPFPJ-AVAL                              PIC X(19).
          10 DCOMW83A-IPSSOA-CONJG                            PIC X(40).
          10 DCOMW83A-CPFPJ-CONJG                             PIC X(19).
          10 DCOMW83A-NOME-TEST                               PIC X(40).
          10 DCOMW83A-CPFPJ-TEST                              PIC X(19).
      *----------------------------------------------------------------*
      *                       Final do Include                         *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                       Final da Geracao                         *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
