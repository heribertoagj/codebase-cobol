      *****************************************************************
      * NOME DA INC - I#GFCTNG                                        *
      * DESCRICAO   - FLEXZ PARA AGRUPAMENTO - LISTA ENCERRAMENTO     *
      *               COMUNICACAO WEB X IMS  - SAIDA - GFCT0882       *
      * TAMANHO     - 1010                                            *
      * GFCTJZ-LL   - 1010                                            *
      * DATA        - 17.03.2006                                      *
      * RESPONSAVEL - VALERIA - PROCWORK                              *
      *****************************************************************
      *
       01  GFCTNG-SAIDA.
           03  GFCTNG-LL                            PIC S9(004) COMP.
           03  GFCTNG-ZZ                            PIC S9(004) COMP.
           03  GFCTNG-TRANSACAO                     PIC  X(009).
           03  GFCTNG-FUNCAO                        PIC  X(008).
           03  GFCTNG-DADOS-RST.
               05  GFCTNG-QTDE-OCOR                 PIC  9(007).
               05  GFCTNG-QTDE-TOT-REG              PIC  9(009).
               05  GFCTNG-FUNC-BDSCO                PIC  X(007).
               05  GFCTNG-FILTRO.
                   07  GFCTNG-AGRUPMTO-FIL          PIC  9(003).
                   07  GFCTNG-TARIFA-FIL            PIC  9(005).
               05  GFCTNG-PONTEIRO.
                   07  GFCTNG-DATA-PONT             PIC  X(010).
                   07  GFCTNG-TARIFA-PONT           PIC  9(005).
                   07  GFCTNG-AGRUPMTO-PONT         PIC  9(003).
                   07  GFCTNG-TIMESTAMP-PONT        PIC  X(026).
           03  GFCTNG-ERRO                          PIC  9(001).
           03  GFCTNG-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTNG-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTNG-DESC-MSG-ERRO                 PIC  X(080).
           03  GFCTNG-FIM                           PIC  X(001).
           03  GFCTNG-FILLER                        PIC  X(026).
           03  GFCTNG-REG OCCURS 4.
                   07  GFCTNG-TIMESTAMP             PIC  X(026).
ST2506*            07  GFCTNG-CGC-CPF               PIC  9(009).
ST2506*            07  GFCTNG-FILIAL                PIC  9(005).
ST2506             07  GFCTNG-CGC-CPF               PIC  X(009).
ST2506             07  GFCTNG-FILIAL                PIC  X(004).
                   07  GFCTNG-CONTROLE              PIC  X(002).
                   07  GFCTNG-AGENCIA               PIC  9(005).
                   07  GFCTNG-POSTO                 PIC  9(005).
                   07  GFCTNG-SEGMENTO              PIC  9(003).
                   07  GFCTNG-MUNICIPIO             PIC  9(007).
                   07  GFCTNG-UF                    PIC  X(002).
                   07  GFCTNG-DESCR-AGPTO           PIC  X(030).
                   07  GFCTNG-COD-TARIFA            PIC  9(005).
                   07  GFCTNG-DESCR-TARIFA          PIC  X(015).
                   07  GFCTNG-SITUACAO              PIC  X(020).
                   07  GFCTNG-DATA-INIC             PIC  X(010).
                   07  GFCTNG-GRUPO-CTBIL           PIC  9(003).
                   07  GFCTNG-SUB-GRUPO-CTBIL       PIC  9(003).
                   07  GFCTNG-DESC-CTBIL            PIC  X(038).
                   07  GFCTNG-ERRO-OCORR            PIC  9(001).
                   07  GFCTNG-COD-MSG-ERRO-OCORR    PIC  9(007).
                   07  GFCTNG-COD-SQL-ERRO-OCORR    PIC  9(003).
