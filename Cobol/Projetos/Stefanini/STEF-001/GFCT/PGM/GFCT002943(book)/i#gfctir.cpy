      *****************************************************************
      * NOME DA INC - I#GFCTIR                                        *
      * DESCRICAO   - UNIFICACAO DE DEBITO - CNPJ/CPF                 *
      *               COMUNICACAO WEB X IMS - SAIDA                   *
      * TAMANHO     - 1010                                            *
      * DATA        - DEZ.2005                                        *
      * RESPONSAVEL - RENATO   - PROCWORK           4152-4352         *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  GFCTIR-SAIDA.
           03  GFCTIR-LL                            PIC S9(004) COMP.
           03  GFCTIR-ZZ                            PIC S9(004) COMP.
           03  GFCTIR-TRANSACAO                     PIC  X(009).
           03  GFCTIR-FUNCAO                        PIC  X(008).
           03  GFCTIR-FILTRO.
ST2507*        07 GFCTIR-CPF-CNPJ                   PIC  9(009).
ST2507*        07 GFCTIR-FILIAL                     PIC  9(005).
ST2507         07 GFCTIR-CPF-CNPJ                   PIC  X(009).
ST2507         07 GFCTIR-FILIAL                     PIC  X(004).
               07 GFCTIR-CTRL                       PIC  9(002).
           03  GFCTIR-NOME                          PIC  X(030).
           03  GFCTIR-QTDE-OCOR                     PIC  9(003).
           03  GFCTIR-QTDE-TOT-REG                  PIC  9(006).
           03  GFCTIR-FUNC-BDSCO                    PIC  X(007).
           03  GFCTIR-DPTO                          PIC  9(005).
           03  GFCTIR-MASTER                        PIC  X(001).
      *        MASTER = 'S'  OU  'N'
           03  GFCTIR-TERMINAL                      PIC  X(008).
           03  GFCTIR-ERRO                          PIC  9(001).
           03  GFCTIR-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTIR-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTIR-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTIR-FIM                           PIC  X(001).
           03  FILLER                               PIC  X(217).
           03  GFCTIR-DADOS.
               05  FILLER OCCURS 5 TIMES.
                   07  GFCTIR-TARIFA                PIC  9(005).
                   07  GFCTIR-DESC-TARIFA           PIC  X(030).
                   07  GFCTIR-COD-MSG-ERRO-GEN      PIC  9(007).
                   07  GFCTIR-DESC-MSG-ERRO-GEN     PIC  X(079).
