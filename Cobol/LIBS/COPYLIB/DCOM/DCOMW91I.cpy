      ******************************************************************
      * NOME BOOK : DCOMW91I - BOOK DE INTERFACE COM SERVICO FUNCIONAL *
      * DESCRICAO : CONSULTA INFORMACOES FORMULARIO ADITAMENTO         *
      * DATA      : 28/03/2023                                         *
      * AUTOR     : RODRIGO PEREIRA                                    *
      * TAMANHO   : 1935 BYTES                                         *
      ******************************************************************
       05 DCOMW91I-HEADER.
         10 DCOMW91I-COD-LAYOUT        PIC X(08)   VALUE 'DCOMW91I'.
         10 DCOMW91I-TAM-LAYOUT        PIC 9(05)   VALUE 1935.
       05 DCOMW91I-REGISTRO.
         10 DCOMW91I-BLOCO-ENTRADA.
           15 DCOMW91I-E-NM-VERSAO                 PIC 9(03).
           15 DCOMW91I-E-TP-IMPRESSAO              PIC 9(02).
           15 DCOMW91I-E-CONTRATO                  PIC 9(13).
           15 DCOMW91I-E-USUARIO                   PIC 9(09).
           15 DCOMW91I-E-AG-USUARIO                PIC 9(05).
         10 DCOMW91I-BLOCO-SAIDA.
           15 DCOMW91I-S-TP-IMPRESSAO              PIC 9(02).
           15 DCOMW91I-S-BANCO                     PIC 9(05).
           15 DCOMW91I-S-NOME-BANCO                PIC X(20).
           15 DCOMW91I-S-CNPJ                      PIC 9(09).
           15 DCOMW91I-S-FILIAL                    PIC 9(04).
           15 DCOMW91I-S-CTRL                      PIC 9(02).
           15 DCOMW91I-S-AGENCIA                   PIC 9(05).
           15 DCOMW91I-S-NOME-AGENCIA              PIC X(20).
           15 DCOMW91I-S-CIDADE-AG                 PIC X(20).
           15 DCOMW91I-S-CONTA-CORRENTE            PIC 9(13).
           15 DCOMW91I-S-DIGITO-CONTA              PIC X(02).
           15 DCOMW91I-S-NOME-CLIENTE              PIC X(40).
           15 DCOMW91I-S-ENDERECO-CLIENTE          PIC X(40).
           15 DCOMW91I-S-CIDADE-CLIENTE            PIC X(20).
           15 DCOMW91I-S-ESTADO-CLIENTE            PIC X(02).
           15 DCOMW91I-S-CPF-CLIENTE               PIC 9(09).
           15 DCOMW91I-S-FILIAL-CLIENTE            PIC 9(04).
           15 DCOMW91I-S-CTRL-CLIENTE              PIC 9(02).
           15 DCOMW91I-S-COD-PRODUTO               PIC 9(03).
           15 DCOMW91I-S-COD-SUBPRODUTO            PIC 9(03).
           15 DCOMW91I-S-COD-CONVENIO              PIC 9(09).
           15 DCOMW91I-S-COD-CONTRATO              PIC 9(09).
           15 DCOMW91I-S-NR-OPERACAO               PIC 9(13).
           15 DCOMW91I-S-NR-SIT-OPERACAO           PIC X(15).
           15 DCOMW91I-S-DATA-OPERACAO             PIC 9(08).
           15 DCOMW91I-S-IND-ADITAMENTO            PIC X(01).
           15 DCOMW91I-S-DOC-ADITAMENTO            PIC X(10).
           15 DCOMW91I-S-DESC-ADITAMENTO           PIC X(15).
           15 DCOMW91I-S-NVIAS-ADITAMENTO          PIC 9(02).
           15 DCOMW91I-S-NOCOR                     PIC 9(02).
           15 DCOMW91I-S-QTD-TOT-PCELAS    OCCURS 30 TIMES.
              20 DCOMW91I-S-NUM-PARCELA            PIC 9(05).
              20 DCOMW91I-S-NOME-SACADO            PIC X(40).
              20 DCOMW91I-S-DATA-VENC-ORIG         PIC 9(08).
              20 DCOMW91I-S-DATA-VENC-PRRG         PIC 9(08).
              20 DCOMW91I-S-VALOR-ENCARGO          PIC 9(15)V99.
              20 DCOMW91I-S-VLR-TIT-EXT-NV         PIC X(200).
              20 DCOMW91I-S-VLR-TIT-EXT-AT         PIC X(200).
              20 DCOMW91I-S-NUM-TITULO             PIC 9(18).
              20 DCOMW91I-S-VLR-TIT-AT             PIC 9(15)V99.
              20 DCOMW91I-S-VLT-TIT-NV             PIC 9(15)V99.
              20 DCOMW91I-S-NOME-SAC-AT            PIC X(40).
              20 DCOMW91I-S-CNPJ-CPF-SAC-AT        PIC 9(09).
              20 DCOMW91I-S-FILIAL-SAC-AT          PIC 9(04).
              20 DCOMW91I-S-CTRL-SAC-AT            PIC 9(02).
              20 DCOMW91I-S-ENDERC-SAC-AT          PIC X(40).
              20 DCOMW91I-S-CNPJ-CPF-SAC-NV        PIC 9(09).
              20 DCOMW91I-S-FILIAL-SAC-N           PIC 9(04).
              20 DCOMW91I-S-CTRL-SAC-N             PIC 9(02).
              20 DCOMW91I-S-ENDERC-SAC-NV          PIC X(40).
              20 DCOMW91I-S-TAX-JUROS-AM-AT        PIC 9(6)V9(7).
              20 DCOMW91I-S-TAX-JUROS-AA-AT        PIC 9(6)V9(7).
              20 DCOMW91I-S-TAX-JUROS-AM-NV        PIC 9(6)V9(7).
              20 DCOMW91I-S-TAX-JUROS-AA-NV        PIC 9(6)V9(7).
              20 DCOMW91I-S-NOME-AVAL-AT           PIC X(40).
              20 DCOMW91I-S-CNPJ-AVAL-AT           PIC 9(09).
              20 DCOMW91I-S-FILIAL-AVAL-AT         PIC 9(04).
              20 DCOMW91I-S-CTRL-AVAL-AT           PIC 9(02).
              20 DCOMW91I-S-NOME-AVAL-NV           PIC X(40).
              20 DCOMW91I-S-CNPJ-AVAL-NV           PIC 9(09).
              20 DCOMW91I-S-FILIAL-AVAL-NV         PIC 9(04).
              20 DCOMW91I-S-CTRL-AVAL-NV           PIC 9(02).
         10  DCOMW91I-RETORNO.
           15 DCOMW91I-S-COD-RETORNO               PIC 9(02).
           15 DCOMW91I-LOC-RET               PIC  9(02).
           15 DCOMW91I-MSG-RET               PIC  X(79).
           15 DCOMW91I-ERRO-AREA             PIC  X(107).
           15 DCOMW91I-SQLCA                 PIC  X(136).
