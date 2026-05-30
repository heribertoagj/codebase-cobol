      ******************************************************************
      * NOME BOOK : CRURWG8I - BOOK DE INTERFACE DO SERVICO CORDENADOR *
      * DESCRICAO :                                                    *
      * DATA      : 17/01/2012                                         *
      * AUTOR     : DCSP                                               *
      * EMPRESA   : CPM BRAXIS                                         *
      * GRUPO     : CPM BRAXIS                                         *
      * COMPONENTE:                                                    *
      * TAMANHO   : 6115 BYTES                                         *
      *********************** DADOS DE ENTRADA    **********************
      * NRO-CPF-CNPJ.......= NUMERO CPF/CNPJ CLIENTE                   *
      * FILIAL-CNPJ........= CODIGO FILIAL CNPJ                        *
      * CCTRL-CPF-CLIE.....= CODIGO CONTROLE CPF CLIENTE               *
      * CUSUAR.............= USUARIO LOGADO VINDO DO FRAMEWORK         *
      ************************ DADOS DE SAIDA **************************
      * NUM-OCCORS.......: NUMERO DE CONSULTAS EFETUADAS               *
      * REG-SAIDA........: QUANTIDADE DE REGISTRO SAIDA                *
      * CPSSOA...........: CODIGO UNICO PESSOA CLIENTE CLUB            *
      * NRO-CPF-CNPJ.....: NUMERO CPF/CNPJ CLIENTE                     *
      * FILIAL-CNPJ......: CODIGO FILIAL CNPJ                          *
      * CTRL-CPF-CNPJ....: DIGITO DE CONTROLE DO CPF/CNPJ              *
      * NM-RAZAO-SOCIAL..: NOME/RAZAO SOCIAL                           *
      * CFICHA-CAD-RURAL.: CODIGO IDENTIFICADOR FICHA CADASTRAL RURAL  *
      * CSIT-FICHA-RURAL.: CODIGO SITUACAO FICHA CADASTRAL RURAL       *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 CRURWG8I-HEADER.
              10 CRURWG8I-COD-LAYOUT    PIC X(08)   VALUE 'CRURWG8I'.
              10 CRURWG8I-TAM-LAYOUT    PIC 9(05)   VALUE  6115.
           05 CRURWG8I-REGISTRO.
              10 CRURWG8I-BLOCO-ENTRADA.
                15 CRURWG8I-E-MAX-OCCOR              PIC 9(005).
CNPJ71          15 CRURWG8I-E-NRO-CPF-CNPJ-X         PIC X(009).
.               15 FILLER REDEFINES CRURWG8I-E-NRO-CPF-CNPJ-X.
.                  20 CRURWG8I-E-NRO-CPF-CNPJ        PIC 9(009).
.               15 CRURWG8I-E-FILIAL-CNPJ-X          PIC X(004).
.               15 FILLER REDEFINES CRURWG8I-E-FILIAL-CNPJ-X.
CNPJ71             20 CRURWG8I-E-FILIAL-CNPJ         PIC 9(004).
                15 CRURWG8I-E-CTRL-CPF-CNPJ          PIC 9(002).
                15 CRURWG8I-E-CUSUAR                 PIC X(009).
                15 CRURWG8I-E-VALD-CPF-CNPJ          PIC X(001).
              10 CRURWG8I-P-BLOCO-PAGINACAO.
                 15 CRURWG8I-INDICADOR-PAGINACAO     PIC X(01).
                   88 CRURWG8I-FUNCAO-VALIDA         VALUES 'I' 'P' 'S'
                                                            'A' 'U'.
                   88 CRURWG8I-CONSULTA-INICIAL      VALUE 'I'.
                   88 CRURWG8I-PRIMEIRO-BLOCO        VALUE 'P'.
                   88 CRURWG8I-BLOCO-SEGUINTE        VALUE 'S'.
                   88 CRURWG8I-BLOCO-ANTERIOR        VALUE 'A'.
                   88 CRURWG8I-ULTIMO-BLOCO          VALUE 'U'.
                 15 CRURWG8I-CHAVE-INI.
                   20 CRURWG8I-INI-PROGRAMA-CHAM     PIC X(08).
                   20 CRURWG8I-INI-PROGRAMA-FUNC     PIC X(08).
                   20 CRURWG8I-CHAVE-INI-CHV.
                      25 CRURWG8I-INI-CCLUB          PIC 9(10).
                      25 CRURWG8I-INI-CEMPR          PIC 9(10).
CNPJ71                25 CRURWG8I-INI-CD-CPFCNPJ-X   PIC X(09).
.                     25 FILLER REDEFINES CRURWG8I-INI-CD-CPFCNPJ-X.
.                        30 CRURWG8I-INI-CD-CPFCNPJ  PIC 9(09).
.                     25 CRURWG8I-INI-FILIAL-CPFCNPJ-X  PIC X(04).
.                     25 FILLER REDEFINES CRURWG8I-INI-FILIAL-CPFCNPJ-X.
CNPJ71                   30 CRURWG8I-INI-FILIAL-CPFCNPJ PIC 9(04).
                      25 CRURWG8I-INI-CTRL-CPFCNPJ   PIC 9(02).
                      25 CRURWG8I-INI-CCTRO-CUSTO    PIC X(04).
                      25 CRURWG8I-INI-CTPO-OBRIG     PIC 9(01).
                      25 CRURWG8I-INI-CPRODT-CREDT   PIC 9(03).
                      25 CRURWG8I-INI-CSPROD-CREDT   PIC 9(04).
                      25 CRURWG8I-INI-CSEQ-TPO-OBRIG PIC 9(03).
                 15 CRURWG8I-CHAVE-FIM.
                   20 CRURWG8I-FIM-CCLUB             PIC 9(10).
                   20 CRURWG8I-FIM-CEMPR             PIC 9(10).
CNPJ71             20 CRURWG8I-FIM-CD-CPFCNPJ-X      PIC X(09).
.                  20 FILLER REDEFINES CRURWG8I-FIM-CD-CPFCNPJ-X.
.                     25 CRURWG8I-FIM-CD-CPFCNPJ     PIC 9(09).
.                  20 CRURWG8I-FIM-FILIAL-CPFCNPJ-X  PIC X(04).
.                  20 FILLER REDEFINES CRURWG8I-FIM-FILIAL-CPFCNPJ-X.
CNPJ71                25 CRURWG8I-FIM-FILIAL-CPFCNPJ PIC 9(04).
                   20 CRURWG8I-FIM-CTRL-CPFCNPJ      PIC 9(02).
                   20 CRURWG8I-FIM-CCTRO-CUSTO       PIC X(04).
                   20 CRURWG8I-FIM-CTPO-OBRIG        PIC 9(01).
                   20 CRURWG8I-FIM-CPRODT-CREDT      PIC 9(03).
                   20 CRURWG8I-FIM-CSPROD-CREDT      PIC 9(04).
                   20 CRURWG8I-FIM-CSEQ-TPO-OBRIG    PIC 9(03).
              10 CRURWG8I-S-BLOCO-SAIDA.
                 15 CRURWG8I-S-NUM-OCCORS            PIC 9(05).
                 15 CRURWG8I-REG-SAIDA OCCURS 0 TO 50 TIMES
                       DEPENDING ON CRURWG8I-S-NUM-OCCORS.
                   20 CRURWG8I-S-CPSSOA              PIC 9(010).
CNPJ71             20 CRURWG8I-S-NRO-CPF-CNPJ-X      PIC X(009).
.                  20 FILLER REDEFINES CRURWG8I-S-NRO-CPF-CNPJ-X.
.                     25 CRURWG8I-S-NRO-CPF-CNPJ     PIC 9(009).
.                  20 CRURWG8I-S-FILIAL-CNPJ-X       PIC X(004).
.                  20 FILLER REDEFINES CRURWG8I-S-FILIAL-CNPJ-X.
CNPJ71                25 CRURWG8I-S-FILIAL-CNPJ      PIC 9(004).
                   20 CRURWG8I-S-CTRL-CPF-CNPJ       PIC 9(002).
                   20 CRURWG8I-S-NM-RAZAO-SOCIAL     PIC X(070).
                   20 CRURWG8I-S-CFICHA-CAD-RURAL    PIC 9(001).
                   20 CRURWG8I-S-CSIT-FICHA-RURAL    PIC 9(003).
                   20 CRURWG8I-S-DT-ATU-CAD-PESSOA   PIC X(10).
                   20 CRURWG8I-S-DT-VALDC            PIC X(10).
