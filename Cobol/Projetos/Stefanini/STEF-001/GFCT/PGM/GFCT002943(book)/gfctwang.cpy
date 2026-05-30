      ******************************************************************
      * NOME DA INC - GFCTWANG                                         *
      * DESCRICAO   - ARQUIVO CLIEADES PARA O SCCE                     *
      * TAMANHO     - 090                                              *
      * DATA        - 05/2010                                          *
      * RESPONSAVEL - MARCUS VINICIUS          - PROCWORK - GP 50.     *
      *================================================================*
ST2509*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2509*    ANALISTA....:  STEFANINI                                    *
ST2509*    DATA........:  09 / 2025                                    *
ST2509*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  GFCTWANG-ENTRADA.
           03 WANG-AGENCIA                 PIC  9(005)    COMP-3.
           03 WANG-RAZAO                   PIC  9(005)    COMP-3.
           03 WANG-CONTA                   PIC  9(007)    COMP-3.
           03 WANG-DIG-CTA                 PIC  9(001).
           03 WANG-NOME-CLIENTE            PIC  X(040).
ST2509     03 WANG-CGC-CPF                 PIC  X(009).
ST2509     03 WANG-FILIAL                  PIC  X(004).
           03 WANG-CONTR                   PIC  9(002).
           03 WANG-POSTO-SERV              PIC  9(005)    COMP-3.
           03 FILLER                       PIC  X(020).
