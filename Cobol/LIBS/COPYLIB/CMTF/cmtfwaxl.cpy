      ******************************************************************
      * NOME BOOK : CMTFWAXL - INTERFACE DADOS DE ENTRADA              *
      * DESCRICAO :                                                    *
      * DATA      : 21/02/2011                                         *
      * AUTOR     : ADEMIR BARBOSA JUNIOR                              *
      * EMPRESA   : CPM BRAXIS                                         *
      * GRUPO     : CPM BRAXIS                                         *
      * COMPONENTE:                                                    *
      * TAMANHO   : 13208 BYTES                                        *
      *********************** DADOS DE ENTRADA *************************
      *                                                                *
      * CEMPR.............: CODIGO DA EMPRESA                          *
      * CCLUB.............: CODIGO DO CLUB                             *
      * CPFCNPJ...........: NUMERO DO CPF / CNPJ                       *
      * CFLIAL-CPFCNPJ....: CODIGO DA FILIAL DO CPF / CNPJ             *
      * CTRL-CPFCNPJ......: CODIGO DE CONTROLE DO CPF / CNPJ           *
      * TPO-PSSOA.........: TIPO DE PESSOA                             *
      *                       PESSOA FISICA   'F'                      *
      *                       PESSOA JURIDICA 'J'                      *
      * CCLASS-CAD-PSSOA  : CODIGO DA CLASSE CADASTRAL DA PESSOA       *
      * NOME-RSOCIAL      : NOME DA PESSOA OU RAZAO SOCIAL             *
      * DNASC-FUNDC       : DATA DE NASCIMENTO OU FUNDA  O             *
      * CTPO-TTLAR-CPF    : TITULAR DO CPF                             *
      * DULT-ATULZ-CAD    : INDICADOR DE LIMITE DE CREDITO             *
      *                                                                *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 CMTFWAXL-HEADER.
             10 CMTFWAXL-COD-LAYOUT    PIC X(08)   VALUE 'CMTFWAXL'.
             10 CMTFWAXL-TAM-LAYOUT    PIC 9(05)   VALUE  13208.
           05 CMTFWAXL-REGISTRO.
             10 CMTFWAXL-BLOCO-RETORNO.
               15 CMTFWAXL-COD-RETORNO             PIC 9(02).
               15 CMTFWAXL-COD-ERRO                PIC X(04).
               15 CMTFWAXL-COD-MENSAGEM            PIC X(08).
               15 CMTFWAXL-DESC-MENSAGEM           PIC X(50).
             10 CMTFWAXL-BLOCO-ENTRADA.
               15 CMTFWAXL-E-CEMPR                 PIC 9(10).
               15 CMTFWAXL-E-CPFCNPJ               PIC 9(09).
               15 CMTFWAXL-E-CFLIAL-CPFCNPJ        PIC 9(04).
               15 CMTFWAXL-E-CTRL-CPFCNPJ          PIC 9(02).
               15 CMTFWAXL-E-TPO-PSSOA             PIC X(01).
             10 CMTFWAXL-BLOCO-SAIDA.
               15 CMTFWAXL-NUM-CONSULTAS           PIC 9(05).
               15 CMTFWAXL-S-REG-SAIDA  OCCURS 0 TO 100 TIMES
                         DEPENDING ON CMTFWAXL-NUM-CONSULTAS.
                 20 CMTFWAXL-S-CEMPR               PIC 9(10).
                 20 CMTFWAXL-S-CCLUB               PIC 9(10).
                 20 CMTFWAXL-S-CPFCNPJ             PIC 9(09).
                 20 CMTFWAXL-S-CFLIAL-CPFCNPJ      PIC 9(04).
                 20 CMTFWAXL-S-CTRL-CPFCNPJ        PIC 9(02).
                 20 CMTFWAXL-S-TPO-PSSOA           PIC X(01).
                 20 CMTFWAXL-S-CCLASS-CAD-PSSOA    PIC 9(03).
                 20 CMTFWAXL-S-NOME-RSOCIAL        PIC X(70).
                 20 CMTFWAXL-S-DNASC-FUNDC         PIC X(10).
                 20 CMTFWAXL-S-CTPO-TTLAR-CPF      PIC 9(02).
                 20 CMTFWAXL-S-DULT-ATULZ-CAD      PIC X(10).
