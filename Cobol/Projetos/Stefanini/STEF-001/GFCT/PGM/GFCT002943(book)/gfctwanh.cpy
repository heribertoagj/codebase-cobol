      ******************************************************************
      * NOME DA INC - GFCTWANH                                         *
      * DESCRICAO   - ARQUIVO SSCCECOR PARA O SCCE                     *
      * TAMANHO     - 100                                              *
      * DATA        - 05/2010                                          *
      * RESPONSAVEL - MARCUS VINICIUS          - PROCWORK - GP 50.     *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  GFCTWANH-SCCE.
           03 SCCE-AGENCIA                 PIC  9(005)    COMP-3.
           03 SCCE-DAC-AG                  PIC  X(001).
           03 SCCE-RAZAO                   PIC  9(005)    COMP-3.
           03 SCCE-CONTA                   PIC  9(007)    COMP-3.
           03 SCCE-DAC-CTA                 PIC  X(001).
           03 SCCE-CGC-CPF.
ST2507*       05 SCCE-CGC-PRINCIPAL        PIC  9(009)    COMP-3.
ST2507*       05 SCCE-CGC-FILIAL           PIC  9(005)    COMP-3.
ST2507        05 SCCE-CGC-PRINCIPAL        PIC  X(009).
ST2507        05 SCCE-CGC-FILIAL           PIC  X(004).
              05 SCCE-CGC-CONTROLE         PIC  9(002).
           03 SCCE-NOME-CLIENTE            PIC  X(039).
           03 SCCE-TIPO-OPCAO              PIC  9(001).
           03 SCCE-MODALIDADE              PIC  X(008).
           03 SCCE-DTINI-VIG-AMD           PIC  9(009)    COMP-3.
           03 SCCE-DTFIM-VIG-AMD           PIC  9(009)    COMP-3.
           03 SCCE-CENTRO-CUSTO            PIC  X(004).
           03 SCCE-POSTO-SERVICO           PIC  9(003)    COMP-3.
           03 FILLER                       PIC  X(014).
