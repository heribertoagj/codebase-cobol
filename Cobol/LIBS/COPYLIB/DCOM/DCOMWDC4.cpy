      *----------------------------------------------------------------*
      *                      Inicio da Geracao                         *
      *              DICM-CopyBook-Processo Publicacao                 *
      *----------------------------------------------------------------*
      * Copy Book - DCOMWDC4
      * Data/Hora : 19/Apr/2023 12:56:39
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                       Inicio do Include                        *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
       05 WDCM-AG-CONTRANTE                                    PIC 9(5).
       05 WDCM-DIG-AG-CONTRANTE                                PIC X(1).
       05 WDCM-CTA-CONTRANTE                                  PIC 9(13).
       05 WDCM-DIG-CTA-CONTRANTE                               PIC X(1).
       05 WDCM-CPF-CONTRATANTE                                PIC X(20).
       05 WDCM-DT-EMITT-CONTR-CDULA                           PIC X(10).
       05 WDCM-VR-PRINC-CONTR                PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
       05 WDCM-NU-DOCTO                                       PIC 9(15).
       05 WDCM-NOME-EMITENTE                                 PIC X(100).
       05 WDCM-CPF                                            PIC X(20).
       05 WDCM-END-EMITENTE                                   PIC X(70).
       05 WDCM-CIDADE-EMITENTE                                PIC X(40).
       05 WDCM-UF-EMITENTE                                     PIC X(2).
       05 WDCM-AG-EMITENTE                                     PIC 9(5).
       05 WDCM-DIG-AG-EMITENTE                                 PIC X(1).
       05 WDCM-NOME-AG-EMITENTE                               PIC X(40).
       05 WDCM-CTA-EMITENTE                                   PIC 9(13).
       05 WDCM-DIG-CTA-EMITENTE                                PIC X(1).
       05 WDCM-DADOS-FIADOR
           OCCURS 4 TIMES.
          10 WDCM-NOME-FIADOR                                 PIC X(70).
          10 WDCM-CPF-CNPJ-FIADOR                             PIC X(20).
          10 WDCM-DOC-TIPO                                     PIC X(3).
          10 WDCM-NM-DOC                                      PIC X(13).
          10 WDCM-ORGAO-EMISSOR                                PIC X(3).
          10 WDCM-UF-RG                                        PIC X(2).
          10 WDCM-END-FIADOR                                  PIC X(70).
          10 WDCM-CIDADE-FIADOR                               PIC X(50).
          10 WDCM-UF-FIADOR                                    PIC X(2).
       05 WDCM-VR-LIMITE                     PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
       05 WDCM-VR-LIMITE-EXTENSO                             PIC X(200).
       05 WDCM-OP-SEMANA                                       PIC X(1).
       05 WDCM-LOCAL-EMISSAO                                  PIC X(40).
       05 WDCM-DT-EMISSAO                                     PIC X(10).
       05 WDCM-NR-VIAS                                         PIC 9(2).
       05 WDCM-NOME-FIADOR-ASSINA1                            PIC X(40).
       05 WDCM-NOME-FIADOR-ASSINA2                            PIC X(40).
       05 WDCM-NOME-FIADOR-ASSINA3                            PIC X(40).
       05 WDCM-NOME-FIADOR-ASSINA4                            PIC X(40).
       05 WDCM-NOME-AVAL-ASS-CONJ1                            PIC X(40).
       05 WDCM-CPF-CNPJ-AVAL-CONJ1                            PIC X(20).
       05 WDCM-NOME-AVAL-ASS-CONJ2                            PIC X(40).
       05 WDCM-CPF-CNPJ-AVAL-CONJ2                            PIC X(20).
       05 WDCM-NOME-TESTEMUNHA1                               PIC X(40).
       05 WDCM-CPF-CNPJ-TESTEMUNHA1                           PIC X(20).
       05 WDCM-NOME-AVAL-TESTEMUNHA2                          PIC X(40).
       05 WDCM-CPF-CNPJ-TESTEMINHA2                           PIC X(20).
      *----------------------------------------------------------------*
      *                       Final do Include                         *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                       Final da Geracao                         *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
