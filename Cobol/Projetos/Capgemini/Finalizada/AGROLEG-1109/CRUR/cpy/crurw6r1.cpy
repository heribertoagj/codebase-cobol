      *---------------------------------------------------------------*
      *                         B R Q                                 *
      *---------------------------------------------------------------*
      * INCLUDE.....: I#COR6R1                                        *
      *               COPIA DO I#RCOR?? - COR0006R1                   *
      * GERACAO.....: DEZEMBRO/2017                                   *
      *---------------------------------------------------------------*
      * OBJETIVO....: DETALHAMENTO DA SISMSG PARA COR0006R1 DO RECOR  *
      *               RESPOSTA A CONSULTA EXTRATO DE OPERACOES DE UM  *
      *               BENEFICIARIO                                    *
      *---------------------------------------------------------------*
4.13  * JUNHO/2019  : SICOR 4.13                                      *
4.13  *               INCLUSAO DO CAMPO QTD-CED                       *
      *---------------------------------------------------------------*
BR0521* 26/05/2021 BRQ               SICOR 5.02                        *
BR0521*----------------------------------------------------------------*
WIP001* 26/10/2022 WIPRO             SICOR 5.05                        *
WIP001*----------------------------------------------------------------*
STE001* MAIO/2024  STEFANINI         SICOR 5.08                        *
STE001*----------------------------------------------------------------*
       01 COR6R1-COR0006R1-1.
          03 COR6R1-COD-MSG         PIC X(009).
          03 COR6R1-NRO-CTRL-IF     PIC X(020).
          03 COR6R1-CNPJ-ENT-RESP   PIC 9(014).
4.13      03 COR6R1-QTD-CED         PIC 9(009).
      *
       01 COR6R1-COR0006R1-2.
          03 COR6R1-REPET-COR0006R1-OP.
             05 COR6R1-GRUPO-COR0006R1-OP.
                10 FILLER                  PIC  X(006) VALUE '{1000}'.
BR0521          10 COR6R1-CNPJ-ENT-RESP-OR             PIC 9(014).
BR0521          10 COR6R1-CNPJ-PART                    PIC 9(014).
                10 COR6R1-NUM-REF-BCCOR                PIC X(011).
                10 COR6R1-NUM-ORDEM-DESTC-OR           PIC 9(003).
                10 COR6R1-DT-EMS                       PIC X(008).
                10 COR6R1-DT-VENC                      PIC X(008).
                10 COR6R1-COD-PROG-LINHA-CRED          PIC X(004).
                10 COR6R1-TP-FNTE-REC                  PIC X(004).
                10 COR6R1-COD-ENPNMNT                  PIC X(014).
STE001          10 COR6R1-COD-SIST-PRODC               PIC X(013).
                10 COR6R1-COD-MOD-SEGR                 PIC 9(001).
                10 COR6R1-VLR-PARCL-CRED               PIC X(019).
                10 COR6R1-VLR-PARCL-REC-PROPRIO        PIC X(019).
4.12 ********** O VLR-PARCL-MANUT-FAMLR PASSOU A SER VALOR GARANTIA
4.12 ********** RENDA MINIMA (VLRGARRENDAMIN)
                10 COR6R1-VLR-PARCL-MANUT-FAMLR        PIC X(019).
                10 COR6R1-VLR-PARCL-COOPERD            PIC X(019).
                10 COR6R1-COD-PROG-LINHA-CRED-COO      PIC X(004).
4.12            10 COR6R1-COD-SUB-PROG                 PIC X(004).
4.11            10 COR6R1-TP-PESSOA-COOPERD            PIC X(001).
4.11            10 COR6R1-CNPJ-CPF-COOPERD             PIC 9(014).
4.11            10 COR6R1-TP-PESSOA-BENFCRIO           PIC X(001).
4.11            10 COR6R1-CNPJ-CPF-BENFCRIO            PIC 9(014).
5.0             10 COR6R1-CNPJ-BASE                    PIC X(008).
5.0             10 COR6R1-NREF-BCOR                    PIC X(011).
5.0             10 COR6R1-VL-PCELA-INVST               PIC X(019).
      *
       01 COR6R1-COR0006R1-3.
          03 COR6R1-DTHR-BC         PIC X(014).
          03 COR6R1-DATA-MOVTO      PIC X(008).
