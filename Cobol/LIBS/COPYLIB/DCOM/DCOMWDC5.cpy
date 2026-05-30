      *----------------------------------------------------------------*
      *                      Inicio da Geracao                         *
      *              DICM-CopyBook-Processo Publicacao                 *
      *----------------------------------------------------------------*
      * Copy Book - DCOMWDC5
      * Data/Hora : 26/Apr/2023 16:26:29
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                       Inicio do Include                        *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
       05 WDCM-AG-CONTRANTE                                    PIC 9(5).
       05 WDCM-DIG-AG-CONTRANTE                                PIC X(1).
       05 WDCM-CTA-CONTRANTE                                  PIC 9(13).
       05 WDCM-DIG-CTA-CONTRANTE                               PIC X(1).
       05 WDCM-CPF-EMITENTE                                   PIC X(20).
       05 WDCM-DT-EMITT-CONTR-CDULA                           PIC X(10).
       05 WDCM-VR-PRINC-CONTR                PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
       05 WDCM-NU-DOCTO                                       PIC 9(15).
       05 WDCM-NOME-EMITENTE                                 PIC X(100).
       05 WDCM-CPF                                            PIC X(20).
       05 WDCM-END-EMITENTE                                   PIC X(70).
       05 WDCM-CIDADE-EMITENTE                                PIC X(40).
       05 WDCM-UF-EMITENTE                                     PIC X(2).
       05 WDCM-DADOS-AVAL
           OCCURS 4 TIMES.
          10 WDCM-NOME-AVAL                                   PIC X(40).
          10 WDCM-CPF-CNPJ-AVAL                               PIC X(20).
          10 WDCM-END-RES-AVAL                                PIC X(40).
          10 WDCM-NM-CIDADE-AVAL                              PIC X(20).
          10 WDCM-SIGLA-UF-AVAL                                PIC X(2).
       05 WDCM-VR-TITULO                     PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
       05 WDCM-VR-TITULOS-EXTENSO                            PIC X(200).
       05 WDCM-DT-BORDERO                                     PIC X(10).
       05 WDCM-DT-VENCIMENTO                                  PIC X(10).
       05 WDCM-AG-BORDERO                                      PIC 9(5).
       05 WDCM-CTA-BORDERO                                    PIC 9(13).
       05 WDCM-CTA                                            PIC 9(13).
       05 WDCM-DIG-CTA                                         PIC X(1).
       05 WDCM-TX-JURO-EFETIVO-MES                  PIC ZZZ.ZZ9,9999999.
       05 WDCM-TX-JURO-EFETIVO-ANO                  PIC ZZZ.ZZ9,9999999.
       05 WDCM-VR-NT-PROMISSORIA             PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
       05 WDCM-AG-BORDERO-DE                                   PIC 9(5).
       05 WDCM-CTA-BORDERO-DE                                 PIC 9(13).
       05 WDCM-VR-TITULO-DE                  PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
       05 WDCM-VR-TITULOS-EXTENSO-DE                         PIC X(200).
       05 WDCM-TX-JURO-EFETIVO-MES-DE               PIC ZZZ.ZZ9,9999999.
       05 WDCM-TX-JURO-EFETIVO-ANO-DE               PIC ZZZ.ZZ9,9999999.
       05 WDCM-DT-VENCIMENTO-DE                               PIC X(10).
       05 WDCM-NOME-AVAL-DE                                   PIC X(40).
       05 WDCM-NOME-AVAL-DE1                                  PIC X(40).
       05 WDCM-NOME-ANUENTE1                                  PIC X(40).
       05 WDCM-NOME-ANUENTE2                                  PIC X(40).
       05 WDCM-LOCAL-EMISSAO                                  PIC X(40).
       05 WDCM-DT-EMISSAO                                     PIC X(10).
       05 WDCM-NOME-AVAL-ASSINA1                              PIC X(40).
       05 WDCM-CPF-CNPJ-AVAL-ASSINA1                          PIC X(20).
       05 WDCM-NOME-AVAL-ASSINA2                              PIC X(40).
       05 WDCM-CPF-CNPJ-AVAL-ASSINA2                          PIC X(20).
       05 WDCM-NOME-AVAL-ASSINA3                              PIC X(40).
       05 WDCM-CPF-CNPJ-AVAL-ASSINA3                          PIC X(20).
       05 WDCM-NOME-AVAL-ASSINA4                              PIC X(40).
       05 WDCM-CPF-CNPJ-AVAL-ASSINA4                          PIC X(20).
       05 WDCM-NOME-AVAL-ASS-CONJ1                            PIC X(40).
       05 WDCM-CPF-CNPJ-AVAL-CONJ1                            PIC X(20).
       05 WDCM-NOME-AVAL-ASS-CONJ2                            PIC X(40).
       05 WDCM-CPF-CNPJ-AVAL-CONJ2                            PIC X(20).
       05 WDCM-NOME-ANUENTE-ASS1                              PIC X(40).
       05 WDCM-CPF-CNPJ-ANUENTE1                              PIC X(20).
       05 WDCM-NOME-ANUENTE-ASS2                              PIC X(40).
       05 WDCM-CPF-CNPJ-ANUENTE2                              PIC X(20).
       05 WDCM-NOME-TESTEMUNHA1                               PIC X(40).
       05 WDCM-CPF-CNPJ-TESTEMUNHA1                           PIC X(20).
       05 WDCM-NOME-AVAL-TESTEMUNHA2                          PIC X(40).
       05 WDCM-CPF-CNPJ-TESTEMINHA2                           PIC X(20).
       05 WDCM-DADOS-AVAL-NOVO
           OCCURS 4 TIMES.
          10 WDCM-NOME-AVAL-NOVO                              PIC X(40).
          10 WDCM-CPF-CNPJ-AVAL-NOVO                          PIC X(20).
          10 WDCM-END-RES-AVAL-NOVO                           PIC X(40).
          10 WDCM-NM-CIDADE-AVAL-NOVO                         PIC X(20).
          10 WDCM-SIGLA-UF-AVAL-NOVO                           PIC X(2).
       05 WDCM-NOME-AG-EMITENTE                               PIC X(40).
       05 WDCM-DIG-AG-EMITENTE                                 PIC X(1).
       05 WDCM-CAMPO-AGENCIA                                  PIC X(20).
       05 WDCM-CAMPO-CONTA                                    PIC X(20).
       05 WDCM-CAMPO-TITULO                                   PIC X(20).
       05 WDCM-CAMPO-JUROS                                    PIC X(20).
       05 WDCM-VR-NT-PROMISSORIA1            PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
       05 WDCM-VR-NT-PROMISSORIA-EXTENSO                     PIC X(200).
       05 WDCM-DT-ALTERADA-DE                                 PIC X(10).
       05 WDCM-CAMPO-VENCIMENTO                               PIC X(20).
      *----------------------------------------------------------------*
      *                       Final do Include                         *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                       Final da Geracao                         *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
