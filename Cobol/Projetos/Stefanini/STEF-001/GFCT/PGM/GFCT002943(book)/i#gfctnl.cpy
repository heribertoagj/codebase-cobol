      *****************************************************************
      * NOME DA INC - I#GFCTNL                                        *
      * DESCRICAO   - FLEXZ PARA AGRUPAMENTO -  CONSULTA DETALHADA    *
      *               COMUNICACAO WEB X IMS  -  ENTRADA - GFCT0884    *
      * TAMANHO     - 300                                             *
      * GFCTNL-LL   - 300                                             *
      * DATA        - 17.03.2006                                      *
      * RESPONSAVEL - VALERIA  - PROCWORK                             *
      *****************************************************************
      *
       01  GFCTNL-ENTRADA.
           03  GFCTNL-LL                            PIC S9(004) COMP.
           03  GFCTNL-ZZ                            PIC S9(004) COMP.
           03  GFCTNL-TRANSACAO                     PIC  X(009).
           03  GFCTNL-FUNCAO                        PIC  X(008).
           03  GFCTNL-DADOS-RST.
               05  GFCTNL-QTDE-OCOR                 PIC  9(007).
               05  GFCTNL-QTDE-TOT-REG              PIC  9(009).
               05  GFCTNL-FUNC-BDSCO                PIC  X(007).
               05  FILLER                           PIC  X(156).
               05  GFCTNL-CHAVE-PEDIDO.
                   07  GFCTNL-AGRUPMTO-PED          PIC  9(003).
                   07  GFCTNL-TARIFA-PED            PIC  9(005).
                   07  GFCTNL-TIMESTAMP-PED         PIC  X(026).
                   07  GFCTNL-SEQUENCIA-PED         PIC  9(002).
               05  GFCTNL-CHAVE-QUENTE.
                   07  GFCTNL-DATA                  PIC  X(010).
                   07  GFCTNL-TARIFA                PIC  9(005).
                   07  GFCTNL-AGRUPMTO              PIC  9(003).
               05  GFCTNL-DADOS.
ST2506*            07  GFCTNL-CGC-CPF               PIC  9(009).
ST2506*            07  GFCTNL-FILIAL                PIC  9(005).
ST2506             07  GFCTNL-CGC-CPF               PIC  X(009).
ST2506             07  GFCTNL-FILIAL                PIC  X(004).
                   07  GFCTNL-CONTROLE              PIC  X(002).
                   07  GFCTNL-AGENCIA               PIC  9(005).
                   07  GFCTNL-POSTO                 PIC  9(005).
                   07  GFCTNL-SEGMENTO              PIC  9(003).
                   07  GFCTNL-MUNICIPIO             PIC  9(007).
                   07  GFCTNL-UF                    PIC  X(002).
                   07  GFCTNL-FIM                   PIC  X(001).
                   07  GFCTNL-TIPO-CONSULTA         PIC  X(001).
      * V = PEDIDOS VIGENTES
      * H = HISTORICO
      * R = RENOVACAO
      * E = ENCERRAMENTO
      * C = CANCELAMENTO
                   07  GFCTNL-GRUPO-CTBIL           PIC  9(003).
                   07  GFCTNL-SUB-GRUPO-CTBIL       PIC  9(003).
