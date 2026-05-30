      *****************************************************************
      *               COMUNICACAO IMS X WEB                   - SAIDA *
      * NOME DA INC - GFCTWAPF                                        *
      * DESCRICAO   - GESTAO - DEMONSTRATIVO DE EVENTOS DO CLIENTE    *
      *                        LISTA DE IMPEDIMENTOS       - GFCT5302 *
      * TAMANHO     - 1010                                            *
      *                186 (FIX0)                                     *
      *                103 (VARIAVEL; 08X)                            *
      * DATA        - NOV/2010                                        *
      * RESPONSAVEL - FRANCISCO LOURENCO              - SONDAPROCWORK *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  WAPF-SAIDA.
           03  WAPF-LL                      PIC S9(004) COMP.
           03  WAPF-ZZ                      PIC S9(004) COMP.
           03  WAPF-TRANSACAO               PIC  X(009).
           03  WAPF-FUNCAO                  PIC  X(008).
           03  WAPF-FILTRO.
               05  WAPF-FLT-CROTNA-ORIGE-MOVTO
                                            PIC  X(004).
               05  WAPF-FLT-DENVIO-MOVTO-TARIF
                                            PIC  X(010).
               05  WAPF-FLT-CNRO-ARQ-MOVTO  PIC  9(002).
               05  WAPF-FLT-CSEQ-MOVTO      PIC  9(011).
           03  WAPF-PONTEIRO.
               05  WAPF-PTR-REG-DESPREZA    PIC  9(005).
           03  WAPF-QTDE-OCOR               PIC  9(003).
           03  WAPF-QTDE-TOT-REG            PIC  9(006).
           03  WAPF-FUNC-BDSCO              PIC  X(007).
           03  WAPF-FIM                     PIC  X(001).
           03  WAPF-ERRO                    PIC  9(001).
           03  WAPF-COD-MSG-ERRO            PIC  9(007).
           03  WAPF-COD-SQL-ERRO            PIC  9(003).
           03  WAPF-DESC-MSG-ERRO           PIC  X(079).
           03  FILLER                       PIC  X(026).
           03  WAPF-DADOS.
               05  FILLER                   OCCURS 08.
                   07  WAPF-DATA-CONS       PIC  X(010).
                   07  WAPF-AGENCIA-CONS    PIC  9(005).
                   07  WAPF-CONTA-CONS      PIC  9(013).
ST2507*            07  WAPF-CNPJ-CPF-CONS   PIC  9(009).
ST2507*            07  WAPF-FILIAL-CNPJ-CONS
ST2507*                                     PIC  9(004).
ST2507             07  WAPF-CNPJ-CPF-CONS   PIC  X(009).
ST2507             07  WAPF-FILIAL-CNPJ-CONS
ST2507                                      PIC  X(004).
                   07  WAPF-CTRL-CNPJ-CPF-CONS
                                            PIC  9(002).
                   07  WAPF-RESULTADO-CONS  PIC  X(060).
