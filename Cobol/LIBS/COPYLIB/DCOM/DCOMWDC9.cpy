      *----------------------------------------------------------------*
      *                      Inicio da Geracao                         *
      *              DICM-CopyBook-Processo Publicacao                 *
      *----------------------------------------------------------------*
      * Copy Book - DCOMWDC9
      * Data/Hora : 17/Apr/2023 14:55:21
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                       Inicio do Include                        *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
       05 WDCM-AG-CONTRANTE                                    PIC 9(5).
       05 WDCM-DIG-AG-CONTRANTE                                PIC X(1).
       05 WDCM-NOME-AG-EMITENTE                               PIC X(20).
       05 WDCM-CTA-CONTRANTE                                  PIC 9(13).
       05 WDCM-DIG-CTA-CONTRANTE                               PIC X(1).
       05 WDCM-CPF-EMITENTE                                   PIC X(20).
       05 WDCM-DT-EMITT-CONTR-CDULA                           PIC X(10).
       05 WDCM-VR-PRINC-CONTR                PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
       05 WDCM-NU-DOCTO                                       PIC 9(15).
       05 WDCM-NOME-EMITENTE                                 PIC X(100).
       05 WDCM-VR-IOF                              PIC Z.ZZZ.ZZ9,999999.
       05 WDCM-VR-JUROS                            PIC Z.ZZZ.ZZ9,999999.
       05 WDCM-VR-SOMA-TOT                   PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
       05 WDCM-VR-LIQUIDO-TOT                PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
       05 WDCM-NOCOR                                           PIC 9(2).
       05 WDCM-OCORENCIA
           OCCURS 50 TIMES.
          10 WDCM-CD-LANC                                      PIC X(8).
          10 WDCM-AG-CHQ                                       PIC 9(5).
          10 WDCM-BANCO-CHQ                                    PIC 9(3).
          10 WDCM-CTA-CHQ                                     PIC 9(13).
          10 WDCM-CPF-CHQ                                     PIC X(20).
          10 WDCM-NCHQ                                         PIC 9(7).
          10 WDCM-DT-DEPOS                                    PIC X(10).
          10 WDCM-VR-JUROS-CHQ                     PIC Z.ZZZ.ZZ9,999999.
          10 WDCM-VR-IOF-CHQ                       PIC Z.ZZZ.ZZ9,999999.
          10 WDCM-VR-LIQUIDO-CHQ             PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
          10 WDCM-VR-TARIFA-TITULO-CHQ       PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
      *----------------------------------------------------------------*
      *                       Final do Include                         *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                       Final da Geracao                         *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
