      ******************************************************************
      * NOME BOOK : DCOMWZ9I                                           *
      * DESCRICAO : BOOK DE INTERFACE FEMP3Z5O                         *
      * FUNCAO    : MONTAGEM DO LAYOUT DICM FEMPWDC4 PARA IMPRESSAO    *
      * DATA      : 04/04/2023                                         *
      * AUTOR     : RODRIGO PEREIRA                                    *
      ******************************************************************
         05 DCOMWZ9I-HEADER.
            10 DCOMWZ9I-COD-LAYOUT          PIC X(08) VALUE 'DCOMWZ9I'.
            10 DCOMWZ9I-TAM-LAYOUT          PIC 9(05) VALUE 23013.

         05 DCOMWZ9I-BLOCO-ENTRADA.

      *******DADOS DO FORMULARIO, VIA E ASSINATURA *********************
            10 DCOMWZ9I-E-CTPLAT-IMPRE-DOCTO
                                            PIC 9(04).
            10 DCOMWZ9I-E-CSGL-TPLAT-IMPRE  PIC X(05).
            10 DCOMWZ9I-E-NUMERO-VIA        PIC 9(01).

      *******DADOS VINDOS DO MODULO CMFE3ICR ***************************

         05 DCOMW91I-BLOCO-SAIDA.
            10 DCOMW91I-S-TP-IMPRESSAO              PIC 9(02).
            10 DCOMW91I-S-BANCO                     PIC 9(05).
            10 DCOMW91I-S-NOME-BANCO                PIC X(20).
            10 DCOMW91I-S-CNPJ                      PIC 9(09).
            10 DCOMW91I-S-FILIAL                    PIC 9(04).
            10 DCOMW91I-S-CTRL                      PIC 9(02).
            10 DCOMW91I-S-AGENCIA                   PIC 9(05).
            10 DCOMW91I-S-NOME-AGENCIA              PIC X(20).
            10 DCOMW91I-S-CIDADE-AG                 PIC X(20).
            10 DCOMW91I-S-CONTA-CORRENTE            PIC 9(13).
            10 DCOMW91I-S-DIGITO-CONTA              PIC X(02).
            10 DCOMW91I-S-NOME-CLIENTE              PIC X(40).
            10 DCOMW91I-S-ENDERECO-CLIENTE          PIC X(40).
            10 DCOMW91I-S-CIDADE-CLIENTE            PIC X(20).
            10 DCOMW91I-S-ESTADO-CLIENTE            PIC X(02).
            10 DCOMW91I-S-CPF-CLIENTE               PIC 9(09).
            10 DCOMW91I-S-FILIAL-CLIENTE            PIC 9(04).
            10 DCOMW91I-S-CTRL-CLIENTE              PIC 9(02).
            10 DCOMW91I-S-COD-PRODUTO               PIC 9(03).
            10 DCOMW91I-S-COD-SUBPRODUTO            PIC 9(03).
            10 DCOMW91I-S-COD-CONVENIO              PIC 9(09).
            10 DCOMW91I-S-COD-CONTRATO              PIC 9(09).
            10 DCOMW91I-S-NR-OPERACAO               PIC 9(13).
            10 DCOMW91I-S-NR-SIT-OPERACAO           PIC X(15).
            10 DCOMW91I-S-DATA-OPERACAO             PIC 9(08).
            10 DCOMW91I-S-IND-ADITAMENTO            PIC X(01).
            10 DCOMW91I-S-DOC-ADITAMENTO            PIC X(10).
            10 DCOMW91I-S-DESC-ADITAMENTO           PIC X(15).
            10 DCOMW91I-S-NVIAS-ADITAMENTO          PIC 9(02).
            10 DCOMW91I-S-NOCOR                     PIC 9(02).
         05 DCOMW91I-BLOCO-PARCELA.
            10 DCOMW91I-S-NUM-PARCELA            PIC 9(05).
            10 DCOMW91I-S-NOME-SACADO            PIC X(40).
            10 DCOMW91I-S-DATA-VENC-ORIG         PIC 9(08).
            10 DCOMW91I-S-DATA-VENC-PRRG         PIC 9(08).
            10 DCOMW91I-S-VALOR-ENCARGO          PIC 9(15)V99.
            10 DCOMW91I-S-VLR-TIT-EXT-NV         PIC X(200).
            10 DCOMW91I-S-VLR-TIT-EXT-AT         PIC X(200).
            10 DCOMW91I-S-NUM-TITULO             PIC 9(18).
            10 DCOMW91I-S-VLR-TIT-AT             PIC 9(15)V99.
            10 DCOMW91I-S-VLT-TIT-NV             PIC 9(15)V99.
            10 DCOMW91I-S-NOME-SAC-AT            PIC X(40).
            10 DCOMW91I-S-CNPJ-CPF-SAC-AT        PIC 9(09).
            10 DCOMW91I-S-FILIAL-SAC-AT          PIC 9(04).
            10 DCOMW91I-S-CTRL-SAC-AT            PIC 9(02).
            10 DCOMW91I-S-ENDERC-SAC-AT          PIC X(40).
            10 DCOMW91I-S-CNPJ-CPF-SAC-NV        PIC 9(09).
            10 DCOMW91I-S-FILIAL-SAC-N           PIC 9(04).
            10 DCOMW91I-S-CTRL-SAC-N             PIC 9(02).
            10 DCOMW91I-S-ENDERC-SAC-NV          PIC X(40).
            10 DCOMW91I-S-TAX-JUROS-AM-AT        PIC 9(6)V9(7).
            10 DCOMW91I-S-TAX-JUROS-AA-AT        PIC 9(6)V9(7).
            10 DCOMW91I-S-TAX-JUROS-AM-NV        PIC 9(6)V9(7).
            10 DCOMW91I-S-TAX-JUROS-AA-NV        PIC 9(6)V9(7).
            10 DCOMW91I-S-NOME-AVAL-AT           PIC X(40).
            10 DCOMW91I-S-CNPJ-AVAL-AT           PIC 9(09).
            10 DCOMW91I-S-FILIAL-AVAL-AT         PIC 9(04).
            10 DCOMW91I-S-CTRL-AVAL-AT           PIC 9(02).
            10 DCOMW91I-S-NOME-AVAL-NV           PIC X(40).
            10 DCOMW91I-S-CNPJ-AVAL-NV           PIC 9(09).
            10 DCOMW91I-S-FILIAL-AVAL-NV         PIC 9(04).
            10 DCOMW91I-S-CTRL-AVAL-NV           PIC 9(02).
      *******AREA DE SAIDA FORMATADA CONFORME DICM *********************
         05 DCOMWZ9I-BLOCO-SAIDA.
            10 DCOMWZ9I-S-AREA-DICM         PIC X(20000).
