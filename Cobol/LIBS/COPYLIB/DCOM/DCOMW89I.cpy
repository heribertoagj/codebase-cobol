      ******************************************************************
      * NOME BOOK : DCOMW89I - BOOK DE INTERFACE COM SERVICO FUNCIONAL *
      * DESCRICAO : CONSULTA INFORMACOES FORMULARIO CONTRATO LIMITE    *
      * DATA      : 11/10/2017                                         *
      * AUTOR     : PATRICK SCARAMAL                                   *
      * EMPRESA   : CD - ALPHA - WIPRO                                 *
      * GRUPO     : 494 - CD ALPHA                                     *
      * TAMANHO   : 1935 BYTES                                         *
      ******************************************************************
       05 DCOMW89I-HEADER.
         10 DCOMW89I-COD-LAYOUT        PIC X(08)   VALUE 'DCOMW89I'.
         10 DCOMW89I-TAM-LAYOUT        PIC 9(05)   VALUE 1935.
       05 DCOMW89I-REGISTRO.
         10 DCOMW89I-BLOCO-ENTRADA.
           15 DCOMW89I-E-CONTRATO                  PIC 9(13).
           15 DCOMW89I-E-USUARIO                   PIC X(09).
         10 DCOMW89I-BLOCO-SAIDA.
           15 DCOMW89I-S-NOME-CLIENTE              PIC X(40).
           15 DCOMW89I-S-NR-CONTRATO               PIC 9(13).
           15 DCOMW89I-S-BANCO                     PIC 9(05).
           15 DCOMW89I-S-NOME-BANCO                PIC X(70).
           15 DCOMW89I-S-CNPJ                      PIC 9(09).
           15 DCOMW89I-S-FILIAL                    PIC 9(04).
           15 DCOMW89I-S-CTRL                      PIC 9(02).
           15 DCOMW89I-S-AGENCIA                   PIC 9(05).
           15 DCOMW89I-S-DIGITO-AGENCIA            PIC X(02).
           15 DCOMW89I-S-NOME-AGENCIA              PIC X(50).
           15 DCOMW89I-S-CONTA-CORRENTE            PIC 9(13).
           15 DCOMW89I-S-DIGITO-CONTA              PIC X(02).
           15 DCOMW89I-S-ENDERECO                  PIC X(40).
           15 DCOMW89I-S-CIDADE                    PIC X(20).
           15 DCOMW89I-S-ESTADO                    PIC X(02).
           15 DCOMW89I-S-PRODUTO                   PIC 9(03).
           15 DCOMW89I-S-DESC-PRODUTO              PIC X(40).
           15 DCOMW89I-S-SUBPRODUTOS OCCURS 7 TIMES.
              20 DCOMW89I-S-COD-SUBPRODUTO         PIC 9(03).
              20 DCOMW89I-S-DESC-SUBPRODUTO        PIC X(40).
           15 DCOMW89I-S-VALOR-LIMITE              PIC 9(15)V99.
           15 DCOMW89I-S-VL-CONTRATO-EXTENSO       PIC X(200).
           15 DCOMW89I-S-VL-TARIFA                 PIC 9(13)V99.
           15 DCOMW89I-S-DESCR-TARIFA              PIC X(30).
           15 DCOMW89I-S-PERCENTUAL-TARIFA         PIC 9(07)V9(06).
           15 DCOMW89I-S-DATA-CELEBRACAO           PIC X(10).
           15 DCOMW89I-S-DATA-INICIO               PIC X(10).
           15 DCOMW89I-S-DATA-VENCIMENTO           PIC X(10).
           15 DCOMW89I-S-PRAZO                     PIC 9(03).
           15 DCOMW89I-S-QTD-OCORR                 PIC 9(03).
           15 DCOMW89I-S-FIADOR-TESTEMUNHA OCCURS 04 TIMES.
              20 DCOMW89I-S-NOME-FIADOR            PIC X(40).
              20 DCOMW89I-S-CNPJ-FIADOR            PIC 9(09).
              20 DCOMW89I-S-FILIAL-FIADOR          PIC 9(04).
              20 DCOMW89I-S-CTRL-FIADOR            PIC 9(02).
              20 DCOMW89I-S-RG-FIADOR              PIC X(15).
              20 DCOMW89I-S-ENDERECO-FIADOR        PIC X(40).
              20 DCOMW89I-S-CIDADE-FIADOR          PIC X(20).
              20 DCOMW89I-S-ESTADO-FIADOR          PIC X(02).
              20 DCOMW89I-S-NOME-CONJUGE           PIC X(40).
              20 DCOMW89I-S-CNPJ-CNJG              PIC 9(09).
              20 DCOMW89I-S-FILIAL-CNJG            PIC 9(04).
              20 DCOMW89I-S-CTRL-CNJG              PIC 9(02).
              20 DCOMW89I-S-NOME-TESTEMUNHA        PIC X(40).
              20 DCOMW89I-S-CNPJ-TESTEMUNHA        PIC 9(09).
              20 DCOMW89I-S-FILIAL-TESTEMUNHA      PIC 9(04).
              20 DCOMW89I-S-CTRL-TESTEMUNHA        PIC 9(02).
