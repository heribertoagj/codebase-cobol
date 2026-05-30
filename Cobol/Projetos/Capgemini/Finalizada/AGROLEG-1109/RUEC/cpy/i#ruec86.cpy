      *---------------------------------------------------------------*
      *                         B R Q                                 *
      *---------------------------------------------------------------*
      * INCLUDE.....: I#RUEC86 - PARA MODULO RUEC8850                 *
      *               COPIA DO I#RCOR?? - COR0006R1                   *
      * GERACAO.....: DEZEMBRO/2017                                   *
      *---------------------------------------------------------------*
      * OBJETIVO....: DETALHAMENTO DA SISMSG PARA COR0006R1 DO RECOR  *
      *               RESPOSTA A CONSULTA EXTRATO DE OPERACOES DE UM  *
      *               BENEFICIARIO                                    *
      *---------------------------------------------------------------*
      * PROJETO ....: REFORMULACAO MENSAGERIA BACEN - RECOR           *
STE001*---------------------------------------------------------------*
-     * * ALTERACAO * - SICOR 5.08 - ALTERACAO DO CAMPO CSIST-PROD    *
-     *---------------------------------------------------------------*
-     *   DATA  ....: OUTUBRO/2024                                    *
STE001*---------------------------------------------------------------*
      **
       01 RUEC86-COR0006R1-1.
          03 RUEC86-COD-MSG         PIC X(009).
          03 RUEC86-NRO-CTRL-IF     PIC X(020).
          03 RUEC86-CNPJ-ENT-RESP   PIC 9(014).
4.13      03 RUEC86-QTD-CED         PIC 9(009).
      *
       01 RUEC86-COR0006R1-2.
          03 RUEC86-REPET-COR0006R1-OP.
             05 RUEC86-GRUPO-COR0006R1-OP.
                10 FILLER                  PIC  X(006) VALUE '{1000}'.
5.02            10 RUEC86-CNPJENTRESPONSOR             PIC 9(014).
5.02            10 RUEC86-CNPJPART                     PIC 9(014).
                10 RUEC86-NUM-REF-BCCOR                PIC X(011).
                10 RUEC86-NUM-ORDEM-DESTC-OR           PIC 9(003).
                10 RUEC86-DT-EMS                       PIC X(008).
                10 RUEC86-DT-VENC                      PIC X(008).
                10 RUEC86-COD-PROG-LINHA-CRED          PIC X(004).
                10 RUEC86-TP-FNTE-REC                  PIC X(004).
                10 RUEC86-COD-ENPNMNT                  PIC X(014).
STE001          10 RUEC86-COD-SIST-PRODC               PIC X(013).
                10 RUEC86-COD-MOD-SEGR                 PIC 9(001).
                10 RUEC86-COD-MUNIC                    PIC 9(006).
                10 RUEC86-VLR-PARCL-CRED               PIC X(019).
                10 RUEC86-VLR-PARCL-REC-PROPRIO        PIC X(019).
4.12 ********** O VLR-PARCL-MANUT-FAMLR PASSOU A SER VALOR GARANTIA
4.12 ********** RENDA MINIMA (VLRGARRENDAMIN)
                10 RUEC86-VLR-PARCL-MANUT-FAMLR        PIC X(019).
                10 RUEC86-VLR-PARCL-COOPERD            PIC X(019).
                10 RUEC86-COD-PROG-LINHA-CRED-COO      PIC X(004).
4.12            10 RUEC86-COD-SUB-PROG                 PIC X(004).
                10 RUEC86-TPSSOA-COOP                  PIC X(001).
                10 RUEC86-CNPJ-CPF-COOP                PIC 9(014).
                10 RUEC86-TPBENEFC                     PIC X(001).
                10 RUEC86-CNPJ-CPF-BENEF               PIC 9(014).
                10 RUEC86-CNPJ-PROAGRO                 PIC X(008).
                10 RUEC86-NUM-REF-PROAGRO              PIC X(011).
                10 RUEC86-VLR-PCL-PROAGRO              PIC X(019).
      *
       01 RUEC86-COR0006R1-4.
          03 RUEC86-DTHR-BC         PIC X(014).
          03 RUEC86-DATA-MOVTO      PIC X(008).
