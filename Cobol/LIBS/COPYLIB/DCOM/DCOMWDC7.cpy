      *----------------------------------------------------------------*
      *                      Inicio da Geracao                         *
      *              DICM-CopyBook-Processo Publicacao                 *
      *----------------------------------------------------------------*
      * Copy Book - DCOMWDC7
      * Data/Hora : 17/Apr/2023 11:00:27
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
           OCCURS 2 TIMES.
          10 WDCM-NOME-AVAL-ATU                               PIC X(40).
          10 WDCM-CPF-CNPJ-AVAL-ATU                           PIC X(20).
          10 WDCM-END-RES-AVAL-ATU                            PIC X(40).
          10 WDCM-NM-CIDADE-AVAL-ATU                          PIC X(20).
          10 WDCM-SIGLA-UF-AVAL-ATU                            PIC X(2).
       05 WDCM-DT-BORDERO                                     PIC X(10).
       05 WDCM-DT-VENCIMENTO                                  PIC X(10).
       05 WDCM-CTA-DE                                         PIC 9(13).
       05 WDCM-DIG-CTA-DE                                      PIC X(1).
       05 WDCM-TX-JURO-EFETIVO-M�S-DE               PIC ZZZ.ZZ9,9999999.
       05 WDCM-TX-JURO-EFETIVO-ANO-DE               PIC ZZZ.ZZ9,9999999.
       05 WDCM-VR-TITULOS-DE                             PIC 9(15)V9(2).
       05 WDCM-VR-TITULOS-EXTENSO-DE                         PIC X(200).
       05 WDCM-VR-NT-PROMISSORIA             PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
       05 WDCM-VR-NT-PROMISSORIA-EXTENSO                     PIC X(200).
       05 WDCM-VR-BORDERO-DESCONTO           PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
       05 WDCM-AG-BORDERO                                      PIC 9(5).
       05 WDCM-CTA-BORDERO                                    PIC 9(13).
       05 WDCM-VR-TITULOS-BORDERO            PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
       05 WDCM-DT-VENCIMENTO-BORDERO                          PIC X(10).
       05 WDCM-NOME-AVAL-EXCLUSAO                             PIC X(70).
       05 WDCM-NOME-AVAL-BORDERO                              PIC X(70).
       05 WDCM-VR-TITULOS-ALTERADO           PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
       05 WDCM-NOME-SACADO                                    PIC X(70).
       05 WDCM-END-SACADO                                     PIC X(70).
       05 WDCM-CPF-CNPJ-ALTERADO                              PIC X(20).
       05 WDCM-TX-JURO-EFETIVO-MES-ALT              PIC ZZZ.ZZ9,9999999.
       05 WDCM-TX-JURO-EFETIVO-ANO-ALT              PIC ZZZ.ZZ9,9999999.
       05 WDCM-DT-VENCIMENTO-ALTERADA                         PIC X(10).
       05 WDCM-NR-VIAS                                         PIC 9(2).
       05 WDCM-VR-JUROS                                  PIC 9(15)V9(2).
       05 WDCM-VR-IOF                                    PIC 9(15)V9(2).
       05 WDCM-VR-LIQUIDO                                PIC 9(15)V9(2).
       05 WDCM-VR-TARIFA-TITULO                          PIC 9(15)V9(2).
       05 WDCM-DADOS-AVAL-NOV
           OCCURS 2 TIMES.
          10 WDCM-NOME-AVAL-NOV                               PIC X(40).
          10 WDCM-CPF-CNPJ-AVAL-NOV                           PIC X(20).
          10 WDCM-END-RES-AVAL-NOV                            PIC X(40).
          10 WDCM-NM-CIDADE-AVAL-NOV                          PIC X(20).
          10 WDCM-SIGLA-UF-AVAL-NOV                            PIC X(2).
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
       05 WDCM-CPF-CNPJ-DE                                    PIC X(20).
       05 WDCM-CPF-CNPJ-PARA                                  PIC X(20).
       05 WDCM-LOCAL-EMISSAO                                  PIC X(40).
       05 WDCM-DT-EMISSAO                                     PIC X(10).
       05 WDCM-CAMPO-AGENCIA                                   PIC X(3).
       05 WDCM-CAMPO-CONTA                                     PIC X(3).
       05 WDCM-CAMPO-TITULO                                    PIC X(3).
       05 WDCM-CAMPO-JUROS                                     PIC X(3).
      *----------------------------------------------------------------*
      *                       Final do Include                         *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                       Final da Geracao                         *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
