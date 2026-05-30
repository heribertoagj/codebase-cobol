      *----------------------------------------------------------------*
      *                      Inicio da Geracao                         *
      *              DICM-CopyBook-Processo Publicacao                 *
      *----------------------------------------------------------------*
      * Copy Book - DCOMW87C
      * Data/Hora : 13/Dez/2017 14:23:48
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                       Inicio do Include                        *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
       05 DCOMW87A-OCORRENCIA
           OCCURS 1 TO 4 TIMES
           DEPENDING ON DCOMW87A-NOCOR.
          10 DCOMW87A-CD-LANC                                   PIC X(8)
              VALUE 'DC07L'.
          10 DCOMW87A-ITESTEMUNHA                             PIC X(40).
          10 DCOMW87A-CPFPJ-TEST                              PIC X(19).
          10 DCOMW87A-CRGTEST                                 PIC X(15).
      *----------------------------------------------------------------*
      *                       Final do Include                         *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                       Final da Geracao                         *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
