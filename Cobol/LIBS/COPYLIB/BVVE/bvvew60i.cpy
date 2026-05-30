      ******************************************************************
      * NOME BOOK : BVVEW60I - INTERFACE COORDENADOR/FUNCIONAL         *
      * DESCRICAO : CONSULTAR LISTA DE CREDENCIADORAS E BANDEIRAS      *
      *             PARA OPTIN                                         *
      * DATA      : DEZEMBRO/2021                                      *
      * AUTOR     : MARCELO DIAS - SISCONSULTORIA                      *
      * TAMANHO   : 24044 BYTES                                        *
      *----------------------------------------------------------------*
      * CAMPO                          DESCRICAO                       *
      ******************************************************************
       05 BVVEW60I-HEADER.
          10 BVVEW60I-COD-LAYOUT            PIC  X(08) VALUE 'BVVEW60I'.
          10 BVVEW60I-TAM-LAYOUT            PIC  9(05) VALUE  24044.
      *
       05 BVVEW60I-E-REGISTRO.
          10 BVVEW60I-E-BLOCO-ENTRADA.
             15 BVVEW60I-E-CCPF             PIC  9(09).
             15 BVVEW60I-E-CFLIAL-CNPJ      PIC  9(04).
             15 BVVEW60I-E-CCTRL-CPF        PIC  9(02).
             15 BVVEW60I-E-CBCO             PIC  9(03).
             15 BVVEW60I-E-CAG-BCRIA        PIC  9(05).
             15 BVVEW60I-E-CCTA-BCRIA-CLI   PIC  9(13).
             15 BVVEW60I-E-CCRDRA-ESTBL-COML
                                            PIC  9(04).
      *
       05 BVVEW60I-S-REGISTRO.
          10 BVVEW60I-S-BLOCO-SAIDA.
             15 BVVEW60I-S-QTD-REG          PIC  9(004).
             15 BVVEW60I-S-AREA-SAIDA.
                20 BVVEW60I-S-TABELA  OCCURS  0 TO 400  TIMES
                   DEPENDING     ON   BVVEW60I-S-QTD-REG.
                   25 BVVEW60I-S-CCRDRA-ESTBL-COML
                                            PIC  9(004).
                   25 BVVEW60I-S-IREDZD-CRDRA-ESTBL
                                            PIC  X(020).
                   25 BVVEW60I-S-TABE OCCURS 12  TIMES.
                      30 BVVEW60I-S-CBANDE-VISAO
                                            PIC  9(003).
