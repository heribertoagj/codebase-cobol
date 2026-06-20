      *****************************************************************
      * NOME DA INC - GFCTWABT                                        *
      * DESCRICAO   - EXTRATO ANUAL DE TARIFAS     - LISTA CONSULTA   *
      *               COMUNICACAO WEB X IMS        - ENTRADA GFCT0511 *
      * TAMANHO     - 200                                             *
      * GFCTWABT-LL - 200                                             *
      * DATA        - 20.10.2008                                      *
      * RESPONSAVEL - VALERIA  - SONDA PROCWORK                       *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  GFCTWABT-ENTRADA.
           03  GFCTWABT-LL                       PIC S9(004) COMP.
           03  GFCTWABT-ZZ                       PIC S9(004) COMP.
           03  GFCTWABT-TRANSACAO                PIC  X(009).
           03  GFCTWABT-FUNCAO                   PIC  X(008).
           03  GFCTWABT-DADOS-RST.
               05  GFCTWABT-QTDE-OCOR            PIC  9(003).
               05  GFCTWABT-QTDE-TOT-REG         PIC  9(006).
               05  GFCTWABT-FUNC-BDSCO           PIC  X(007).
               05  GFCTWABT-FILTRO.
ST2507*            07  GFCTWABT-CPF-FIL          PIC  9(009).
ST2507*            07  GFCTWABT-FILIAL-FIL       PIC  9(005).
ST2507             07  GFCTWABT-CPF-FIL          PIC  X(009).
ST2507             07  GFCTWABT-FILIAL-FIL       PIC  X(004).
                   07  GFCTWABT-CONTR-FIL        PIC  X(002).
                   07  GFCTWABT-TIPO-DEPDC-FIL   PIC  X(002).
                   07  GFCTWABT-COD-DEPTO-FIL    PIC  9(005).
               05  GFCTWABT-FIM                  PIC  X(001).
               05  GFCTWABT-PONTEIRO.
                   07  GFCTWABT-CID-CLI-PONT     PIC  X(026).
                   07  GFCTWABT-TABELA-PONT      PIC  X(008).
                   07  GFCTWABT-AGENCIA-PONT     PIC  9(005).
                   07  GFCTWABT-CONTA-PONT       PIC  9(013).
           03  FILLER                            PIC  X(087).
