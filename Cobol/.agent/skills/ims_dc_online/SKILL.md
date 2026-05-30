---
name: ims_dc_online
description: Skills for developing and maintaining COBOL programs in IMS/DC online environment, including DL/I calls, message processing, and MFS.
---

# IMS/DC – COBOL Online

Esta skill orienta o desenvolvimento e a manutenção de programas COBOL no ambiente **IMS/DC** (Data Communications), cobrindo processamento de mensagens, chamadas DL/I e integração com terminais.

## Pré-requisitos

- Conhecimento de COBOL e de **IMS-DB** (banco de dados).
- Entendimento do **PSB** (Program Specification Block), que controla o acesso do programa aos recursos IMS, incluindo **PCBs** (Program Communication Blocks).

## Componentes do ambiente online

| Componente | Descrição |
| :--- | :--- |
| **DB** | Processamento de banco de dados IMS. |
| **DC** | Processamento de comunicações (mensagens, terminais). |
| **DB/DC** | Aplicações que usam banco e comunicações. |
| **I/O PCB** | PCB de entrada/saída para filas de mensagens. |

## Chamadas DL/I para mensagens

O programa deve usar `CALL 'CBLTDLI'` com o **I/O PCB** para ler mensagens da fila.

### Códigos de função principais (mensagens)

| Código | Nome | Uso |
| :--- | :--- | :--- |
| **GU** | Get Unique | Primeiro segmento de uma nova mensagem. |
| **GN** | Get Next | Próximo segmento em mensagem multi-segmento. |
| **GNP** | Get Next Parent | Próximo segmento sob o mesmo segmento pai. |

### Formato da chamada (exemplo)

```cobol
CALL 'CBLTDLI' USING W-PARM-CNT,
                      C-FUNC-CODE,
                      L-IO-PCB,
                      W-MESSAGE-IO
```

> [!IMPORTANT]
> Um **MPP** (Message Processing Program) deve executar **pelo menos um GU** no I/O PCB para obter a mensagem. Caso contrário, pode ocorrer abend de usuário **462**.

### Fluxo típico para mensagens multi-segmento

1. Primeira chamada: **GU** para obter o primeiro segmento.
2. Chamadas seguintes: **GN** para os segmentos restantes.
3. Verificar o **status code** do PCB após cada chamada (em branco = sucesso).

## Banco de dados (DB) – códigos de função

Para operações de banco com possibilidade de bloqueio:

| Código | Nome | Uso |
| :--- | :--- | :--- |
| **GHU** | Get Hold Unique | Leitura com hold (para update/delete). |
| **GHN** | Get Hold Next | Leitura sequencial com hold. |
| **REPL** | Replace | Atualização de segmento previamente lido com hold. |
| **DLET** | Delete | Exclusão de segmento. |
| **ISRT** | Insert | Inserção de segmento. |

## Tópicos de codificação

- **Formato da chamada DL/I** e processamento de mensagens.
- Tratamento de **mensagens de entrada e saída**.
- Posicionamento de cursor e **atributos** de tela.
- **MFS** (Message Format Service) para definição de telas.
- **Modificação dinâmica de atributos** e change calls.
- Programação **conversacional** (múltiplas trocas de mensagem com o terminal).

## Debug e testes

- Ferramentas interativas de debug são mais limitadas que em batch; uso de **DISPLAY** para rastrear lógica é comum.
- **BTS** (Batch Terminal Simulator) permite testar chamadas IMS a partir do TSO.
- Validar sempre o **status code** retornado no PCB após cada chamada DL/I.

## Regras práticas

1. **Sempre** tratar o status do I/O PCB e dos PCBs de banco após cada `CALL 'CBLTDLI'`.
2. Em programas DB/DC, **não** misturar uso incorreto de PCB de banco com I/O PCB (cada um tem sua área e status).
3. Manter **nomenclatura consistente** para PCBs e áreas de mensagem (ex.: sufixos `-PCB`, `-IO`, `-MESSAGE`).
4. Documentar no código qual PCB está associado a cada tipo de recurso (mensagem vs. banco).
