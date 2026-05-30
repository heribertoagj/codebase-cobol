      ******************************************************************
      * NOME BOOK : DCOMWZ6I                                           *
      * DESCRICAO : BOOK DE INTERFACE FEMP3Z5O                         *
      * FUNCAO    : MONTAGEM DO LAYOUT DICM FEMPWDC4 PARA IMPRESSAO    *
      * DATA      : 26/12/2022                                         *
      * AUTOR     : RODRIGO PEREIRA                                    *
      ******************************************************************
         05 DCOMWZ6I-HEADER.
            10 DCOMWZ6I-COD-LAYOUT          PIC X(08) VALUE 'DCOMWZ6I'.
            10 DCOMWZ6I-TAM-LAYOUT          PIC 9(05) VALUE 29380.

         05 DCOMWZ6I-BLOCO-ENTRADA.

      *******DADOS DO FORMULARIO, VIA E ASSINATURA *********************
            10 DCOMWZ6I-E-CTPLAT-IMPRE-DOCTO
                                            PIC 9(04).
            10 DCOMWZ6I-E-CSGL-TPLAT-IMPRE  PIC X(05).
            10 DCOMWZ6I-E-NUMERO-VIA        PIC 9(01).

      *******DADOS VINDOS DO MODULO CMFE3ICR ***************************
            10 DCOMW90I-BLOCO-SAIDA.
               15 DCOMW90I-S-BANCO                     PIC 9(05).
               15 DCOMW90I-S-NOME-BANCO                PIC X(70).
               15 DCOMW90I-S-NOME-CLIENTE              PIC X(40).
               15 DCOMW90I-S-CNPJ                      PIC 9(09).
               15 DCOMW90I-S-FILIAL                    PIC 9(04).
               15 DCOMW90I-S-CTRL                      PIC 9(02).
               15 DCOMW90I-S-ENDERECO                  PIC X(40).
               15 DCOMW90I-S-CIDADE                    PIC X(20).
               15 DCOMW90I-S-ESTADO                    PIC X(02).
               15 DCOMW90I-S-AGENCIA                   PIC 9(05).
               15 DCOMW90I-S-DIGITO-AGENCIA            PIC X(02).
               15 DCOMW90I-S-NOME-AGENCIA              PIC X(50).
               15 DCOMW90I-S-CIDADE-AG                 PIC X(20).
               15 DCOMW90I-S-CONTA-CORRENTE            PIC 9(13).
               15 DCOMW90I-S-DIGITO-CONTA              PIC X(02).
               15 DCOMW90I-S-PRODUTO                   PIC 9(03).
      *        15 DCOMW90I-S-DESC-PRODUTO              PIC X(40).
               15 DCOMW90I-S-COD-SUBPRODUTO            PIC 9(03).
               15 DCOMW90I-S-DESC-SUBPRODUTO           PIC X(40).
               15 DCOMW90I-S-NR-OPERACAO               PIC 9(13).
               15 DCOMW90I-S-NR-SIT-OPERACAO           PIC X(15).
               15 DCOMW90I-S-COD-CONVENIO              PIC 9(09).
               15 DCOMW90I-S-COD-CONTRATO              PIC 9(09).
               15 DCOMW90I-S-VALOR-LIMITE              PIC 9(15)V99.
               15 DCOMW90I-S-VL-CONTRATO-EXTENSO       PIC X(200).
               15 DCOMW90I-S-VL-TARIFA-OP              PIC 9(15)V99.
               15 DCOMW90I-S-VL-TARIFA-TITULO          PIC 9(15)V99.
               15 DCOMW90I-S-VL-JUROS                  PIC 9(15)V99.
               15 DCOMW90I-S-VL-IOF                    PIC 9(15)V99.
               15 DCOMW90I-S-VL-LIQUIDO                PIC 9(15)V99.
               15 DCOMW90I-S-VL-NT-PROMISSORIA         PIC 9(15)V99.
               15 DCOMW90I-S-VL-NT-PROMI-EXTENSO       PIC X(200).
      *        15 DCOMW90I-S-DESCR-TARIFA              PIC X(30).
      *        15 DCOMW90I-S-PERCENTUAL-TARIFA         PIC 9(07)V9(06).
               15 DCOMW90I-S-TIPO-TAXA                 PIC X(15).
               15 DCOMW90I-S-JUROS-MES                 PIC 9(07)V9(06).
               15 DCOMW90I-S-JUROS-ANO                 PIC 9(07)V9(06).
               15 DCOMW90I-S-GARANTIA                  PIC X(40).
               15 DCOMW90I-S-CONE                      PIC 9(18).
               15 DCOMW90I-S-DATA-OPERACAO             PIC X(10).
               15 DCOMW90I-S-DATA-VENCIMENTO           PIC X(10).
               15 DCOMW90I-S-AGENCIA-AT                PIC 9(05).
               15 DCOMW90I-S-DIGITO-AGENCIA-AT         PIC X(02).
               15 DCOMW90I-S-NOME-AGENCIA-AT           PIC X(50).
               15 DCOMW90I-S-CONTA-CORRENTE-AT         PIC 9(13).
               15 DCOMW90I-S-DIGITO-CONTA-AT           PIC X(02).
               15 DCOMW90I-S-NOME-CLIENTE-AT           PIC X(40).
               15 DCOMW90I-S-VALOR-OPERACAO-AT         PIC 9(15)V99.
               15 DCOMW90I-S-VL-OPERACAO-EXTENSO       PIC X(200).
               15 DCOMW90I-S-JUROS-MES-AT              PIC 9(07)V9(06).
               15 DCOMW90I-S-JUROS-ANO-AT              PIC 9(07)V9(06).
               15 DCOMW90I-S-DT-VENCIMENTO-AT          PIC X(10).
               15 DCOMW90I-S-INC-ADITAMENTO            PIC X(01).
               15 DCOMW90I-S-DOC-ADITAMENTO            PIC X(10).
               15 DCOMW90I-S-DESC-ADITAMENTO           PIC X(15).
               15 DCOMW90I-S-NVIAS                     PIC 9(02).
               15 DCOMW90I-S-NOME-TESTEMUNHA-1         PIC X(40).
               15 DCOMW90I-S-CNPJ-TESTEMUNHA-1         PIC 9(09).
               15 DCOMW90I-S-FILIAL-TESTEMUNHA-1       PIC 9(04).
               15 DCOMW90I-S-CTRL-TESTEMUNHA-1         PIC 9(02).
               15 DCOMW90I-S-NOME-TESTEMUNHA-2         PIC X(40).
               15 DCOMW90I-S-CNPJ-TESTEMUNHA-2         PIC 9(09).
               15 DCOMW90I-S-FIL-TESTEMUNHA-2          PIC 9(04).
               15 DCOMW90I-S-CTRL-TESTEMUNHA-2         PIC 9(02).
               15 DCOMW90I-S-NOME-AVAL-1AT             PIC X(40).
               15 DCOMW90I-S-CNPJ-AVAL-1AT             PIC 9(09).
               15 DCOMW90I-S-FILIAL-AVAL-1AT           PIC 9(04).
               15 DCOMW90I-S-CTRL-AVAL-1AT             PIC 9(02).
               15 DCOMW90I-S-ENDERECO-AVAL-1AT         PIC X(40).
               15 DCOMW90I-S-CIDADE-AVAL-1AT           PIC X(20).
               15 DCOMW90I-S-ESTADO-AVAL-1AT           PIC X(02).
               15 DCOMW90I-S-NOME-AVAL-2AT             PIC X(40).
               15 DCOMW90I-S-CNPJ-AVAL-2AT             PIC 9(09).
               15 DCOMW90I-S-FILIAL-AVAL-2AT           PIC 9(04).
               15 DCOMW90I-S-CTRL-AVAL-2AT             PIC 9(02).
               15 DCOMW90I-S-ENDERECO-AVAL-2AT         PIC X(40).
               15 DCOMW90I-S-CIDADE-AVAL-2AT           PIC X(20).
               15 DCOMW90I-S-ESTADO-AVAL-2AT           PIC X(02).
               15 DCOMW90I-S-NOME-AVAL-1NV             PIC X(40).
               15 DCOMW90I-S-CNPJ-AVAL-1NV             PIC 9(09).
               15 DCOMW90I-S-FILIAL-AVAL-1NV           PIC 9(04).
               15 DCOMW90I-S-CTRL-AVAL-1NV             PIC 9(02).
               15 DCOMW90I-S-ENDERECO-AVAL-1NV         PIC X(40).
               15 DCOMW90I-S-CIDADE-AVAL-1NV           PIC X(20).
               15 DCOMW90I-S-ESTADO-AVAL-1NV           PIC X(02).
               15 DCOMW90I-S-NOME-CONJUGE-1NV          PIC X(40).
               15 DCOMW90I-S-CNPJ-CONJUGE-1NV          PIC 9(09).
               15 DCOMW90I-S-FILIAL-CONJUGE-1NV        PIC 9(04).
               15 DCOMW90I-S-CTRL-CONJUGE-1NV          PIC 9(02).
               15 DCOMW90I-S-NOME-AVAL-2NV             PIC X(40).
               15 DCOMW90I-S-CNPJ-AVAL-2NV             PIC 9(09).
               15 DCOMW90I-S-FILIAL-AVAL-2NV           PIC 9(04).
               15 DCOMW90I-S-CTRL-AVAL-2NV             PIC 9(02).
               15 DCOMW90I-S-ENDERECO-AVAL-2NV         PIC X(40).
               15 DCOMW90I-S-CIDADE-AVAL-2NV           PIC X(20).
               15 DCOMW90I-S-ESTADO-AVAL-2NV           PIC X(02).
               15 DCOMW90I-S-NOME-CONJUGE-2NV          PIC X(40).
               15 DCOMW90I-S-CNPJ-CONJUGE-2NV          PIC 9(09).
               15 DCOMW90I-S-FILIAL-CONJUGE-2NV        PIC 9(04).
               15 DCOMW90I-S-CTRL-CONJUGE-2NV          PIC 9(02).
               15 DCOMW90I-S-NOME-AVAL-3NV             PIC X(40).
               15 DCOMW90I-S-CNPJ-AVAL-3NV             PIC 9(09).
               15 DCOMW90I-S-FILIAL-AVAL-3NV           PIC 9(04).
               15 DCOMW90I-S-CTRL-AVAL-3NV             PIC 9(02).
               15 DCOMW90I-S-ENDERECO-AVAL-3NV         PIC X(40).
               15 DCOMW90I-S-CIDADE-AVAL-3NV           PIC X(20).
               15 DCOMW90I-S-ESTADO-AVAL-3NV           PIC X(02).
               15 DCOMW90I-S-NOME-CONJUGE-3NV          PIC X(40).
               15 DCOMW90I-S-CNPJ-CONJUGE-3NV          PIC 9(09).
               15 DCOMW90I-S-FILIAL-CONJUGE-3NV        PIC 9(04).
               15 DCOMW90I-S-CTRL-CONJUGE-3NV          PIC 9(02).
               15 DCOMW90I-S-NOME-AVAL-4NV             PIC X(40).
               15 DCOMW90I-S-CNPJ-AVAL-4NV             PIC 9(09).
               15 DCOMW90I-S-FILIAL-AVAL-4NV           PIC 9(04).
               15 DCOMW90I-S-CTRL-AVAL-4NV             PIC 9(02).
               15 DCOMW90I-S-ENDERECO-AVAL-4NV         PIC X(40).
               15 DCOMW90I-S-CIDADE-AVAL-4NV           PIC X(20).
               15 DCOMW90I-S-ESTADO-AVAL-4NV           PIC X(02).
               15 DCOMW90I-S-NOME-CONJUGE-4NV          PIC X(40).
               15 DCOMW90I-S-CNPJ-CONJUGE-4NV          PIC 9(09).
               15 DCOMW90I-S-FILIAL-CONJUGE-4NV        PIC 9(04).
               15 DCOMW90I-S-CTRL-CONJUGE-4NV          PIC 9(02).
               15 DCOMW90I-S-NOME-SACADO-AT            PIC X(40).
               15 DCOMW90I-S-CNPJ-SACADO-AT            PIC 9(09).
               15 DCOMW90I-S-FILIAL-SACADO-AT          PIC 9(04).
               15 DCOMW90I-S-CTRL-SACADO-AT            PIC 9(02).
               15 DCOMW90I-S-ENDERECO-SACADO-AT        PIC X(40).
               15 DCOMW90I-S-NOME-SACADO-NV            PIC X(40).
               15 DCOMW90I-S-CNPJ-SACADO-NV            PIC 9(09).
               15 DCOMW90I-S-FILIAL-SACADO-NV          PIC 9(04).
               15 DCOMW90I-S-CTRL-SACADO-NV            PIC 9(02).
               15 DCOMW90I-S-ENDERECO-SACADO-NV        PIC X(40).
               15 DCOMW90I-S-JUROS-MES-NV              PIC 9(07)V9(06).
               15 DCOMW90I-S-JUROS-ANO-NV              PIC 9(07)V9(06).
               15 DCOMW90I-S-NOME-SACADO-AVAL          PIC X(40).
               15 DCOMW90I-S-CNPJ-SACADO-AVAL          PIC 9(09).
               15 DCOMW90I-S-FILIAL-SACADO-AVAL        PIC 9(04).
               15 DCOMW90I-S-CTRL-SACADO-AVAL          PIC 9(02).
               15 DCOMW90I-S-NOME-SAC-AVAL-NV          PIC X(40).
               15 DCOMW90I-S-CNPJ-SAC-AVAL-NV          PIC 9(09).
               15 DCOMW90I-S-FIL-SAC-AVAL-NV           PIC 9(04).
               15 DCOMW90I-S-CT-SAC-AVAL-NV            PIC 9(02).
               15 DCOMW90I-S-PRAZO                     PIC 9(03).
               15 DCOMW90I-S-QTD-OCORR                 PIC 9(03).
               15 DCOMW90I-S-FIADOR-TESTEMUNHA OCCURS 04 TIMES.
                  20 DCOMW90I-S-NOME-FIADOR            PIC X(40).
                  20 DCOMW90I-S-CNPJ-FIADOR            PIC 9(09).
                  20 DCOMW90I-S-FILIAL-FIADOR          PIC 9(04).
                  20 DCOMW90I-S-CTRL-FIADOR            PIC 9(02).
                  20 DCOMW90I-S-RG-FIADOR              PIC X(15).
                  20 DCOMW90I-S-ENDERECO-FIADOR        PIC X(40).
                  20 DCOMW90I-S-CIDADE-FIADOR          PIC X(20).
                  20 DCOMW90I-S-ESTADO-FIADOR          PIC X(02).
                  20 DCOMW90I-S-NOME-CONJUGE           PIC X(40).
                  20 DCOMW90I-S-CNPJ-CNJG              PIC 9(09).
                  20 DCOMW90I-S-FILIAL-CNJG            PIC 9(04).
                  20 DCOMW90I-S-CTRL-CNJG              PIC 9(02).
                  20 DCOMW90I-S-NOME-TESTEMUNHA        PIC X(40).
                  20 DCOMW90I-S-CNPJ-TESTEMUNHA        PIC 9(09).
                  20 DCOMW90I-S-FILIAL-TESTEMUNHA      PIC 9(04).
                  20 DCOMW90I-S-CTRL-TESTEMUNHA        PIC 9(02).

      *******AREA DE SAIDA FORMATADA CONFORME DICM *********************
         05 DCOMWZ6I-BLOCO-SAIDA.
            10 DCOMWZ6I-S-AREA-DICM         PIC X(20000).
