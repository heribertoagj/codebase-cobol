      *****************************************************************
      * NOME DA INC - GFCTWABU                                        *
      * DESCRICAO   - EXTRATO ANUAL DE TARIFAS     - LISTA CONSULTA   *
      *               COMUNICACAO WEB X IMS        - SAIDA GFCT0511   *
      * TAMANHO     - 1010                                            *
      * GFCTWABU-LL - 1010                                            *
      * DATA        - 20.10.2008                                      *
      * RESPONSAVEL - VALERIA  - SONDA PROCWORK                       *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                  *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  GFCTWABU-SAIDA.
           03  GFCTWABU-LL                       PIC S9(004) COMP.
           03  GFCTWABU-ZZ                       PIC S9(004) COMP.
           03  GFCTWABU-TRANSACAO                PIC  X(009).
           03  GFCTWABU-FUNCAO                   PIC  X(008).
           03  GFCTWABU-DADOS-RST.
               05  GFCTWABU-QTDE-OCOR            PIC  9(003).
               05  GFCTWABU-QTDE-TOT-REG         PIC  9(006).
               05  GFCTWABU-FUNC-BDSCO           PIC  X(007).
               05  GFCTWABU-FILTRO.
ST2507*            07  GFCTWABU-CPF-FIL          PIC  9(009).
ST2507*            07  GFCTWABU-FILIAL-FIL       PIC  9(005).
ST2507             07  GFCTWABU-CPF-FIL          PIC  X(009).
ST2507             07  GFCTWABU-FILIAL-FIL       PIC  X(004).
                   07  GFCTWABU-CONTR-FIL        PIC  X(002).
                   07  GFCTWABU-TIPO-DEPDC-FIL   PIC  X(002).
                   07  GFCTWABU-COD-DEPTO-FIL    PIC  9(005).
               05  GFCTWABU-PONTEIRO.
                   07  GFCTWABU-CID-CLI-PONT     PIC  X(026).
                   07  GFCTWABU-TABELA-PONT      PIC  X(008).
                   07  GFCTWABU-AGENCIA-PONT     PIC  9(005).
                   07  GFCTWABU-CONTA-PONT       PIC  9(013).
           03  GFCTWABU-ERRO                     PIC  9(001).
           03  GFCTWABU-COD-MSG-ERRO             PIC  9(007).
           03  GFCTWABU-COD-SQL-ERRO             PIC  9(003).
           03  GFCTWABU-DESC-MSG-ERRO            PIC  X(079).
           03  GFCTWABU-FIM                      PIC  X(001).
           03  FILLER                            PIC  X(048).
           03  GFCTWABU-DADOS.
               05  FILLER                        OCCURS 11.
                   07  GFCTWABU-AGENCIA-LISTA    PIC  9(005).
                   07  GFCTWABU-CONTA-LISTA      PIC  9(013).
                   07  GFCTWABU-NOME-LISTA       PIC  X(040).
                   07  GFCTWABU-ERRO-OCORR       PIC  9(001).
                   07  GFCTWABU-COD-MSG-ERRO-OCORR
                                                 PIC  9(007).
                   07  GFCTWABU-COD-SQL-ERRO-OCORR
                                                 PIC  9(003).
