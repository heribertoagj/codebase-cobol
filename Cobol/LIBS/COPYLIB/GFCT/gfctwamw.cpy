      ******************************************************************
      * NOME DA INC - GFCTWAMW                                         *
      * DESCRICAO   - ADESAO INDIVIDUAL - DETALHE DA PERMISSAO DE ADSAO*
      *                                                      GFCT0124  *
      *               COMUNICACAO WEB X IMS        - ENTRADA           *
      * TAMANHO     - 200                                              *
      * DATA        - 03.2010                                          *
      * RESPONSAVEL - MARCUS VINICIUS          - PROCWORK - GP 50.     *
      ******************************************************************
      *
       01  GFCTWAMW-ENTRADA.
           03  WAMW-LL                     PIC S9(004) COMP.
           03  WAMW-ZZ                     PIC S9(004) COMP.
           03  WAMW-TRANSACAO              PIC  X(009).
           03  WAMW-FUNCAO                 PIC  X(008).
           03  WAMW-FUNC-BDSCO             PIC  X(007).
           03  WAMW-DADOS1.
               05  WAMW-COD-TARIFA         PIC  9(005).
               05  WAMW-COD-AGPTO          PIC  9(003).
               05  WAMW-SEQUENCIA          PIC  9(009).
               05  WAMW-DATA-INICIO        PIC  X(010).
               05  WAMW-DATA-FIM           PIC  X(010).
               05  WAMW-FIM                PIC  X(001).
BI0810         05  WAMW-CPSSOA-TARIFA      PIC  X(001).
BI0810*          '1' - FISICA
BI0810*          '2' - JURIDICA
BI0810*          '3' - AMBAS
BI0810         05  FILLER                  PIC  X(133).
