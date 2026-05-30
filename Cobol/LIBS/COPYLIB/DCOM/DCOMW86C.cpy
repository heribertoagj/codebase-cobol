      *----------------------------------------------------------------*
      *                      Inicio da Geracao                         *
      *              DICM-CopyBook-Processo Publicacao                 *
      *----------------------------------------------------------------*
      * Copy Book - DCOMW86C
      * Data/Hora : 21/Dez/2017 14:25:53
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                       Inicio do Include                        *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
       05 DCOMW86A-OCORRENCIA
           OCCURS 1 TO 50 TIMES
           DEPENDING ON DCOMW86A-NOCOR.
          10 DCOMW86A-CD-LANC                                   PIC X(8)
              VALUE 'DC05L'.
          10 DCOMW86A-CBCO-CHQ                                 PIC 9(3).
          10 DCOMW86A-AGEN-CHQ                                 PIC 9(5).
          10 DCOMW86A-CTA-CHQ                                 PIC 9(13).
          10 DCOMW86A-CCPFCNPJ-CHQ                             PIC 9(9).
          10 DCOMW86A-CFILIAL-CHQ                              PIC 9(4).
          10 DCOMW86A-CCONT-CHQ                                PIC 9(2).
          10 DCOMW86A-NCHQ                                     PIC 9(7).
          10 DCOMW86A-DT-DEPOS                                PIC X(10).
          10 DCOMW86A-VIOF-CHQ                                PIC 9(15).
          10 DCOMW86A-VJUR-CHQ                                PIC 9(15).
          10 DCOMW86A-VLIQ-CHQ                                PIC 9(15).
          10 DCOMW86A-VTOT-CHQ                                PIC 9(17).
      *----------------------------------------------------------------*
      *                       Final do Include                         *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                       Final da Geracao                         *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
