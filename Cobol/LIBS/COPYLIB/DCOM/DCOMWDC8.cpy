      *----------------------------------------------------------------*
      *                      Inicio da Geracao                         *
      *              DICM-CopyBook-Processo Publicacao                 *
      *----------------------------------------------------------------*
      * Copy Book - DCOMWDC8
      * Data/Hora : 17/Apr/2023 14:55:05
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
       05 WDCM-VR-TARIFA                     PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
       05 WDCM-VR-TARIFA-DESC                PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
       05 WDCM-VR-IOF                        PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
       05 WDCM-VR-IOF-DESC                   PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
       05 WDCM-VR-JUROS                      PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
       05 WDCM-VR-QTDE-TITULO                             PIC 9(3)V9(2).
       05 WDCM-VR-IOF-TOT                          PIC Z.ZZZ.ZZ9,999999.
       05 WDCM-VR-JUROS-TOT                        PIC Z.ZZZ.ZZ9,999999.
       05 WDCM-VR-TITULO-TOT                 PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
       05 WDCM-VR-LIQUIDO-TOT                PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
       05 WDCM-NOCOR                                           PIC 9(2).
       05 WDCM-OCORENCIA
           OCCURS 50 TIMES.
          10 WDCM-NUMERO-TITULO                               PIC X(20).
          10 WDCM-NOME-FIADOR                                 PIC X(40).
          10 WDCM-CPF-CNPJ-FIADOR                             PIC X(20).
          10 WDCM-DVCTO-TITLO                                 PIC X(10).
          10 WDCM-VR-JUROS1                        PIC Z.ZZZ.ZZ9,999999.
          10 WDCM-VR-IOF1                          PIC Z.ZZZ.ZZ9,999999.
          10 WDCM-VR-LIQUIDO                 PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
          10 WDCM-VR-TARIFA-TITULO           PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
      *----------------------------------------------------------------*
      *                       Final do Include                         *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                       Final da Geracao                         *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
