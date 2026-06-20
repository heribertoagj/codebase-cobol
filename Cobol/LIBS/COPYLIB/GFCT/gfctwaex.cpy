      ******************************************************************
      * NOME DA INC - GFCTWAEX                                         *
      * DESCRICAO   - ARQUIVO DO EXTRATO MENSAL                        *
      * TAMANHO     - 132                                              *
      * DATA        - 12/2008                                          *
      * RESPONSAVEL - VINICIUS C. MADUREIRA - SONDA PROCWORK
      ******************************************************************
      * ALTERADO    - 10/2009
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                  *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*


       01  WAEX-EXTRAMES-HEADER.
           03 WAEX-TIPO-REGISTRO-H      PIC X(001).
      *       'H' = HEADER
           03 WAEX-DATA-PROCESSO-H.
              05 WAEX-MES-PROCESSO-H    PIC 9(002).
              05 WAEX-ANO-PROCESSO-H    PIC 9(004).
           03 WAEX-TIPO-PROCESSO-H      PIC X(001).
      *       'C' = CARGA
      *       'R' = REPROCESSAMENTO
      *    QUANDO -R- FAREMOS O REPROCESSAMENTO DO MES/ANO INDICADO NO H
      *    QUANDO -C- AS INFORMACOES DE MES/ANO DO HEADER PODEM VIR ZERA
           03 WAEX-QTDE-PROCESSADO-H    PIC 9(011).
           03 WAEX-QTDE-REJEITADO-H     PIC 9(011).
           03 FILLER                    PIC X(102).

       01  WAEX-EXTRAMES-DETALHE REDEFINES WAEX-EXTRAMES-HEADER.
           03 WAEX-TIPO-REGISTRO        PIC X(001).
      *       'T' = TARIFAS
      *       'E' = ESTORNO
           03 WAEX-COD-AGENCIA          PIC 9(005).
           03 WAEX-CPF-CNPJ.
ST2507*       05 WAEX-PRINCIPAL         PIC 9(009).
ST2507*       05 WAEX-FILIAL            PIC 9(004).
ST2507        05 WAEX-PRINCIPAL         PIC X(009).
ST2507        05 WAEX-FILIAL            PIC X(004).
              05 WAEX-CTLE              PIC 9(002).
           03 WAEX-RAZAO.
              05 WAEX-GRUPO             PIC 9(003).
              05 WAEX-SUBGP             PIC 9(003).
           03 WAEX-NRO-CONTA            PIC 9(007).
           03 WAEX-NUMERO-DOCUMENTO     PIC 9(007).
           03 WAEX-COD-LANCAMENTO       PIC 9(005).
           03 WAEX-DATA-LANCAMENTO.
              05 WAEX-DT-LANCTO-DD      PIC 9(002).
              05 WAEX-DT-LANCTO-MM      PIC 9(002).
              05 WAEX-DT-LANCTO-AAAA    PIC 9(004).
           03 WAEX-DESC-LANCAMENTO      PIC X(031).
           03 WAEX-VLR-LANCAMENTO.
              05 WAEX-VALOR             PIC 9(012).
              05 FILLER                 PIC 9(001).
              05 WAEX-CENTAVOS          PIC 9(002).
           03 WAEX-HIST-CPL             PIC X(032).

