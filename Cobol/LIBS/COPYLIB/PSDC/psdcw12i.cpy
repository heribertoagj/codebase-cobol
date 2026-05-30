      ******************************************************************
      * NOME BOOK : PSDCW12I - INTERFACE DO COORDENADOR E FUNCIONAL    *
      *                        PSDC312C                                *
      * DATA      : 16/08/2010                                         *
      * AUTOR     : THIAGO ARAUJO FRANCISCO                            *
      * EMPRESA   : CPM BRAXIS                                         *
      * GRUPO     : CPM BRAXIS                                         *
      * COMPONENTE:                                                    *
      * TAMANHO   : 177 BYTES                                        *
      *********************** DADOS DE RETORNO *************************
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 PSDCW12IE-HEADER.
             10 PSDCW12I-COD-LAYOUT    PIC X(08)   VALUE 'PSDCW12I'.
             10 PSDCW12I-TAM-LAYOUT    PIC 9(05)   VALUE 177.
           05 PSDCW12I-REGISTRO.
             10 PSDCW12I-BLOCO-ENTRADA.
               15 PSDCW12I-E-CEMPR                 PIC 9(10).
               15 PSDCW12I-E-CD-CLUB               PIC 9(10).
               15 PSDCW12I-E-CD-FUNC-BRADESCO      PIC 9(09).
               15 PSDCW12I-E-CD-TERMINAL           PIC X(08).
               15 PSDCW12I-E-DEPTO                 PIC 9(05).
             10 PSDCW12I-BLOCO-SAIDA.
                 20 PSDCW12I-S-CD-IDENT-PESSOA     PIC X(01).
                 20 PSDCW12I-S-CD-IDENT-OPERACAO   PIC X(01).
                 20 PSDCW12I-S-CD-EMPRESA          PIC 9(10).
                 20 PSDCW12I-S-CD-CLASSE-CADASTRAL PIC 9(03).
                 20 PSDCW12I-S-CCPF-CNPJ           PIC 9(09).
                 20 PSDCW12I-S-CD-FILIAL-CNPJ      PIC 9(04).
                 20 PSDCW12I-S-CCTRL-CPF-CNPJ      PIC 9(02).
                 20 PSDCW12I-S-NM-RZ-SCIAL         PIC X(70).
                 20 PSDCW12I-S-DNASC-FUNDC         PIC X(10).
                 20 PSDCW12I-S-DT-ATU-CAD-PESSOA   PIC X(10).
                 20 PSDCW12I-S-ST-RLCTO-PESSOA     PIC 9(02).